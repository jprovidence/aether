module Text.Nouns (
    initialPositioning
) where


import Data.Maybe
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.HashTable.IO as M
import Control.Exception
import Control.Monad
import Database.HDBC
import Database.HDBC.PostgreSQL
import Feed
import Viterbi


----------------------------------------------------------------------------------------------------

-- typedefs

type ByteString = B.ByteString
type Table k v = M.CuckooHashTable k v
type Chart = Table ByteString [(ByteString, Int)]


----------------------------------------------------------------------------------------------------

--

data IndexDist = Less10
               | Less100
               | Other


----------------------------------------------------------------------------------------------------

-- Database connection parameters (Password altered for github)

connStr :: String
connStr = "host=localhost dbname=ticket connect_timeout=7 port=5432 user=postgres password=password"


----------------------------------------------------------------------------------------------------

-- Database

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- Document-Graph Conversion

----------------------------------------------------------------------------------------------------

--

documentsToGraph :: Vit -> [ByteString] -> IO ()
documentsToGraph vit docs = mapM_ (documentToGraph vit) docs


----------------------------------------------------------------------------------------------------

--

documentToGraph :: Vit -> ByteString -> IO ()
documentToGraph vit doc = tag vit doc >>= nounsAndIndices >>= integrate


----------------------------------------------------------------------------------------------------

--

integrate :: [(ByteString, Int)] -> IO ()

integrate [] = return ()

integrate [x] = return ()

integrate (x:xs) = mapM_ (integrateX x) xs >> integrate xs


----------------------------------------------------------------------------------------------------

--

integrateX :: (ByteString, Int) -> (ByteString, Int) -> IO ()
integrateX (cur, i) (fut, fi) =
    case cur == fut of
        True  -> return ()
        False -> case idxDist i fi of
                    Less10  -> pushScore 3 cur fut
                    Less100 -> pushScore 2 cur fut
                    Other   -> pushScore 1 cur fut


----------------------------------------------------------------------------------------------------

--

idxDist :: Int -> Int -> IndexDist
idxDist a b
    | (abs $ a - b) <= 10  = Less10
    | (abs $ a - b) <= 100 = Less100
    | otherwise           = Other


----------------------------------------------------------------------------------------------------

--

pushScore :: Int -> ByteString -> ByteString -> IO ()
pushScore i a b = do
    relId <- ensureNounExists a >>= \aid -> ensureNounExists b >>= \bid -> ensureRelExists aid bid
    incrementRelation relId i


----------------------------------------------------------------------------------------------------

--

incrementRelation :: Int -> Int -> IO ()
incrementRelation relId sre = do
    let sel = "select score from edge where id=" ++ (show relId) ++ ";"
        upd x y = "update edge set score=" ++ (show $ x + y) ++ " where id=" ++ (show relId) ++ ";"
    sql <- wrap (genericSingleLookup sel) >>= return . fromJust >>= return . (upd sre)
    wrap (execIncrement sql)
    return ()

    where execIncrement :: String -> Connection -> IO Integer
          execIncrement sql con = withTransaction con (\c -> prepare c sql >>= flip execute [])


----------------------------------------------------------------------------------------------------

--

ensureNounExists :: ByteString -> IO Int
ensureNounExists n = ensure (nounLookup n) (nounCreate n)


----------------------------------------------------------------------------------------------------

--

ensureRelExists :: Int -> Int -> IO Int
ensureRelExists vtxa vtxb = do
    probe <- relLookup vtxa vtxb >>= \trya -> relLookup vtxb vtxa >>= \tryb -> return (trya, tryb)
    case probe of
        (Nothing, Nothing) -> relCreate vtxa vtxb
        (Just id, Nothing) -> return id
        (Nothing, Just id) -> return id
        (Just _, Just _)   -> putStrLn "> Error in #ensureRelExists" >> return (-1)


----------------------------------------------------------------------------------------------------

--

ensure :: IO (Maybe Int) -> IO Int -> IO Int
ensure lookupF createF =
    lookupF >>= \mid ->
    case mid of
        Nothing -> join (evaluate createF) >>= return
        Just x  -> return x


----------------------------------------------------------------------------------------------------

--

nounLookup :: ByteString -> IO (Maybe Int)
nounLookup n =
    let sel = "select id from vertex where noun='" ++ (B.unpack n) ++ "';"
    in wrap (genericSingleLookup sel)


----------------------------------------------------------------------------------------------------

--

relLookup :: Int -> Int -> IO (Maybe Int)
relLookup vtxa vtxb =
    let sel = "select id from edge where nodea=" ++ (show vtxa) ++ " and nodeb=" ++ (show vtxb) ++ ";"
    in wrap (genericSingleLookup sel)


----------------------------------------------------------------------------------------------------

--

nounCreate :: ByteString -> IO Int
nounCreate n =
    let ins = "insert into vertex (noun) values ('" ++ (B.unpack n) ++ "');"
        sel = "select id from vertex where noun='" ++ (B.unpack n) ++ "';"
    in wrap (genericCreateWid ins sel)


----------------------------------------------------------------------------------------------------

--

relCreate :: Int -> Int -> IO Int
relCreate vtxa vtxb =
    let ins = "insert into edge (nodea, nodeb) values (" ++ (show vtxa) ++ ", " ++ (show vtxb) ++ ");"
        sel = "select id from edge where nodea=" ++ (show vtxa) ++ " and nodeb=" ++ (show vtxb) ++ ";"
    in wrap (genericCreateWid ins sel)


----------------------------------------------------------------------------------------------------

--

genericSingleLookup :: String -> Connection -> IO (Maybe Int)
genericSingleLookup sel con =
    quickQuery' con sel [] >>= \rows ->
    case rows of
        [] -> return Nothing
        _  -> mapM (return . fromSql . (flip (!!) 0)) rows >>= return . Just . flip (!!) 0


----------------------------------------------------------------------------------------------------

--

genericCreateWid :: String -> String -> Connection -> IO Int
genericCreateWid ins sel con = do
    withTransaction con (\c -> prepare c ins >>= flip execute [])
    rows <- quickQuery' con sel []
    mapM (return . fromSql . (flip (!!) 0)) rows >>= return . fromJust . flip (!!) 0




----------------------------------------------------------------------------------------------------

-- Clustering interface

----------------------------------------------------------------------------------------------------

-- number of vertices in the graph

numVertices :: IO Int
numVertices = let sel = "select count(id) from vertex;"
              in wrap (genericSingleLookup sel) >>= return . fromJust


----------------------------------------------------------------------------------------------------

-- build a pseudo-grid (@len@ x @len@) of points given the total number of points total

initialPositioning :: Int -> Float -> [[(Float, Float)]]
initialPositioning numPts len =
    let rt = sqrt $ fromIntegral numPts
        rndUp = ceiling rt
        rndDwn = floor rt
    in case rndUp == rndDwn of
           True  -> coordsPerfectSq len rndUp
           False -> coordsMismatch numPts len rndDwn rndUp


coordsPerfectSq :: Float -> Int -> [[(Float, Float)]]
coordsPerfectSq len bound =
    let mod = len / (fromIntegral bound)
        coords = take bound [ mod * i | i <- [0..] ]
    in L.map (L.zip coords . L.repeat) coords


coordsMismatch :: Int -> Float -> Int -> Int -> [[(Float, Float)]]
coordsMismatch pts len rndDown rndUp
    | willOverflow pts rndUp =
        let mod = len / (fromIntegral rndUp)
            coords = take rndUp [ mod * i | i <- [0..] ]

            -- final row
            countF = pts - (rndUp * (rndUp - 1))
            modF = len / (fromIntegral countF)
            coordsF = take countF [ modF * i | i <- [0..] ]

        in (L.map (zip coords . repeat) $ L.init coords) ++ [(zip coordsF (repeat $ L.last coords))]

    | otherwise =
        let modX = len / (fromIntegral rndDown)
            modY = len / (fromIntegral rndUp)
            coordsX = take rndDown [ modX * i | i <- [0..] ]
            coordsY = take rndUp [ modY * i | i <- [0..] ]

            -- final row
            countF = pts - (rndDown^2)
            modF = len / (fromIntegral countF)
            coordsFX = take countF [ modF * i | i <- [0..] ]

        in (L.map (zip coordsX . repeat) $ L.init coordsY) ++ [(zip coordsFX (repeat $ L.last coordsY))]


    where willOverflow :: Int -> Int -> Bool
          willOverflow pts u = pts > (u * (u - 1))

