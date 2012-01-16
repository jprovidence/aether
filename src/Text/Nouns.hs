module Text.Nouns (
    initialPositioning
) where


import Data.Maybe
import Data.Int
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.HashTable.IO as M
import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import System.IO
import Database.HDBC
import Database.HDBC.PostgreSQL
import Network.Socket
import Foreign.Storable
import Foreign.Marshal.Alloc
import Feed
import Viterbi


----------------------------------------------------------------------------------------------------

-- typedefs

type ByteString = B.ByteString
type Table k v = M.CuckooHashTable k v
type Chart = Table ByteString [(ByteString, Int)]


----------------------------------------------------------------------------------------------------

-- represents the generalized distance (number of separating words) between two nouns in a document

data IndexDist = Less10
               | Less100
               | Other


----------------------------------------------------------------------------------------------------

-- represents a point in the cluster-space

newtype Point2D = Point2D { coords :: (Float, Float) -- position of the point in a 2D cluster-space
                          } deriving Show


-- data type to facilitate cache of relationship strengths,
-- the vertex-origin of this relationship is implicit

data Strength = Strength { target   :: Int  -- the other vertex on this edge
                         , strength :: Int  -- measure of relationship strength
                         } deriving Show


-- data type to facilitate caching of relationships on each noun

data RelationCache = RCache { related   :: [Strength]
                            , unrelated :: [Int]
                            } deriving Show


newtype ForceFunc = ForceFunc { applyF :: (Int -> Float -> Float) }


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

genericLookup :: String -> Connection -> IO (Maybe [Int])
genericLookup sel con =
    quickQuery' con sel [] >>= \rows ->
    case rows of
        [] -> return Nothing
        _  -> return $ Just $ L.map (fromSql . (flip (!!) 0)) rows


----------------------------------------------------------------------------------------------------

--

genericSingleLookup :: String -> Connection -> IO (Maybe Int)
genericSingleLookup sel con =
    genericLookup sel con >>= return . liftM (flip (!!) 0)


----------------------------------------------------------------------------------------------------

--

genericCreateWid :: String -> String -> Connection -> IO Int
genericCreateWid ins sel con = do
    withTransaction con (\c -> prepare c ins >>= flip execute [])
    rows <- quickQuery' con sel []
    return (L.map (fromSql . (flip (!!) 0)) rows) >>= return . fromJust . flip (!!) 0




----------------------------------------------------------------------------------------------------

-- Clustering interface

----------------------------------------------------------------------------------------------------

--

runCluster :: Float -> IO ()
runCluster dimen = do
    let moveMax = (sqrt ((dimen^2) + (dimen^2))) / 100
    nchan <- newChan
    edges <- cacheEdges
    count <- numVertices
    verts <- cacheVertices $ initialPositioning dimen count
    scMax <- scoreMax
    attrF <- return $ configAttractiveMod linearA moveMax scMax
    replF <- return $ configRepulsiveMod moveMax (moveMax * 5)
    forkIO $ waitVisualizationReq nchan
    iterativeClustering nchan edges verts attrF replF


iterativeClustering :: Chan (Table Int Point2D) ->
                      Table Int RelationCache ->
                      Table Int Point2D ->
                      ForceFunc ->
                      ForceFunc ->
                      IO ()
iterativeClustering chan edges verts attrF replF = do
    reportNewPositions chan verts
    nVerts <- clusterCycle edges verts attrF replF
    iterativeClustering chan edges nVerts attrF replF

----------------------------------------------------------------------------------------------------

-- apply all forces to each vertex once

clusterCycle :: Table Int RelationCache -> Table Int Point2D -> ForceFunc -> ForceFunc ->
                IO (Table Int Point2D)
clusterCycle edges verts attrF replF = do
    newVerts <- M.new
    edgeList <- M.toList edges
    mapM_ (\edj -> reposition newVerts verts edj attrF replF) edgeList
    return newVerts


----------------------------------------------------------------------------------------------------

-- reposition a vertex according to the forces acting upon it

reposition :: Table Int Point2D -> Table Int Point2D -> (Int, RelationCache) -> ForceFunc -> ForceFunc ->
              IO ()
reposition tbl verts (id, rcache) attrF repulF = do
    let cleanLookup x = liftM fromJust $ M.lookup verts x

    curPos <- cleanLookup id

    rels <- mapM (prepCache verts) $ related rcache
    unrels <- mapM cleanLookup (unrelated rcache) >>= \xs -> return $ L.zip xs $ repeat 0

    tempPos <- return $ L.foldl' release curPos $ L.map (coil attrF) rels
    finalPos <- return $ L.foldl' release tempPos $ L.map (coil repulF) unrels

    M.insert tbl id finalPos

    where prepCache :: Table Int Point2D -> Strength -> IO (Point2D, Int)
          prepCache tbl s = M.lookup tbl (target s) >>= \pt -> return (fromJust pt, strength s)

          coil :: ForceFunc -> (Point2D, Int) -> (Point2D -> Point2D)
          coil ff (pt, scr) = applyForce scr ff pt

          release :: Point2D -> (Point2D -> Point2D) -> Point2D
          release acc f = f acc


----------------------------------------------------------------------------------------------------

-- calculate the new position of a vertex

applyForce :: Int -> ForceFunc -> Point2D -> Point2D -> Point2D
applyForce score (ForceFunc ff) (Point2D (xa, ya)) (Point2D (xb, yb)) =
    let curDist = pythagoras (xa - xb) (ya - yb)
        move = ff score curDist
        moveRatio = move / curDist

    in case moveRatio == 0 of
        True  -> Point2D (xb, yb)
        False -> Point2D ((moveRatio * (xa - xb)) + xb, (moveRatio * (ya - yb)) + yb)

    where pythagoras :: Float -> Float -> Float
          pythagoras a b = sqrt ((a^2) + (b^2))


----------------------------------------------------------------------------------------------------

-- configure a repulsive force function for easy passing

configRepulsiveMod :: Float -> Float -> ForceFunc
configRepulsiveMod moveMax thresh = ForceFunc (modRepulsiveForce moveMax thresh)


----------------------------------------------------------------------------------------------------

-- configure an attractive force function for easy passing

configAttractiveMod :: (Float -> Float -> Float) -> Float -> Int -> ForceFunc
configAttractiveMod f moveMax scoreMax = ForceFunc (modAttractiveForce f moveMax scoreMax)


----------------------------------------------------------------------------------------------------

-- modulate the repulsive move according to a linear function

modRepulsiveForce :: Float -> Float -> Int -> Float -> Float
modRepulsiveForce moveMax thresh _ dist =
    let trespass = 1 - (dist / thresh)
    in case dist < thresh of
           False -> 0
           True  -> trespass * moveMax


----------------------------------------------------------------------------------------------------

-- modulate the attractive force on a vertex according to distance and the chosen mod function

modAttractiveForce :: (Float -> Float -> Float) -> Float -> Int -> Int -> Float -> Float
modAttractiveForce f moveMax scoreMax score dist =
    let fscore = fromIntegral score
        fscoreMax = fromIntegral scoreMax

        naiveMove = (fscore / fscoreMax) * moveMax  -- move distance before modulation
        scHypotenuse = moveMax * 100.0              -- largest distance possible
        moveMultiplier = f dist scHypotenuse        -- apply modulating function

    in naiveMove * moveMultiplier


----------------------------------------------------------------------------------------------------

-- modulating function for attractive forces, linear

linearA :: Float -> Float -> Float
linearA a b = a / b


----------------------------------------------------------------------------------------------------

-- modulating function for attractive forces, exponential

exponentialA :: Float -> Float -> Float
exponentialA a b = (a^2) / (b^2)


----------------------------------------------------------------------------------------------------

-- modulating function for repulsive forces, opposite exponential

fracExponentialA :: Float -> Float -> Float
fracExponentialA a b = (sqrt a) / (sqrt b)


----------------------------------------------------------------------------------------------------

-- load edge data into memory from database

cacheEdges :: IO (Table Int RelationCache)
cacheEdges = do
    let sel = "select id from vertex;"
    allIds <- vertexIds
    htbl <- M.new
    mapM_ (\id -> relationSummary' id allIds >>= M.insert htbl id) allIds
    return htbl


----------------------------------------------------------------------------------------------------

-- load vertex data into memory from database

cacheVertices :: [Point2D] -> IO (Table Int Point2D)
cacheVertices pts = do
    ids <- vertexIds
    tbl <- M.new
    mapM_ (\(id, pt) -> M.insert tbl id pt) $ zip ids pts
    return tbl


----------------------------------------------------------------------------------------------------

-- list of all noun database ids

vertexIds :: IO [Int]
vertexIds = let sel = "select id from vertex;"
            in wrap (genericLookup sel) >>= return . fromJust


----------------------------------------------------------------------------------------------------

-- number of vertices in the graph

numVertices :: IO Int
numVertices = let sel = "select count(id) from vertex;"
              in wrap (genericSingleLookup sel) >>= return . fromJust


----------------------------------------------------------------------------------------------------

-- determine the highest current relation score

scoreMax :: IO Int
scoreMax = let sel = "select score from edge order by score desc limit 1;"
           in wrap (genericSingleLookup sel) >>= return . fromJust


----------------------------------------------------------------------------------------------------

-- given its id, load all edges on a vertex

relationSummary :: Int -> IO RelationCache
relationSummary id = vertexIds >>= relationSummary' id


----------------------------------------------------------------------------------------------------

-- all edges on a vertex, takes an @allIds@ parameter to avoid repeditive lookups

relationSummary' :: Int -> [Int] -> IO RelationCache
relationSummary' id allIds = do
    related <- allRelated id
    unrelated <- allUnrelated' id allIds related
    strengths <- mapM (strengthBetween id) related
    return $ RCache strengths unrelated


----------------------------------------------------------------------------------------------------

-- the ids of all nouns related to the given id

allRelated :: Int -> IO [Int]
allRelated id = do
    let sela = "select nodeb from edge where nodea=" ++ (show id) ++ ";"
        selb = "select nodea from edge where nodeb=" ++ (show id) ++ ";"
    resa <- wrap (genericLookup sela)
    resb <- wrap (genericLookup selb)
    return $ L.concat $ catMaybes [resa, resb]


----------------------------------------------------------------------------------------------------

-- the ids of all unrelated vertices

allUnrelated :: Int -> IO [Int]
allUnrelated id =
    let sel = "select id from vertex;"
    in wrap (genericLookup sel) >>= \lst -> allRelated id >>= \excl -> allUnrelated' id (fromJust lst) excl


----------------------------------------------------------------------------------------------------

-- all unrelated vertices, takes a list of all ids and all ids with which there are relations to
-- avoid repeditive lookups

allUnrelated' :: Int -> [Int] -> [Int] -> IO [Int]
allUnrelated' id lst excl = return $ L.filter (\x -> not (x `L.elem` excl)) lst


----------------------------------------------------------------------------------------------------

-- load the unmodulated strength of attraction between two nodes

strengthBetween :: Int -> Int -> IO Strength
strengthBetween a b = do
    let sela = "select score from edge where nodea=" ++ (show a) ++ " and nodeb=" ++ (show b) ++ ";"
        selb = "select score from edge where nodea=" ++ (show b) ++ " and nodeb=" ++ (show a) ++ ";"
    resa <- wrap (genericSingleLookup sela)
    resb <- wrap (genericSingleLookup selb)
    return $ case (resa, resb) of
                 (Just score, Nothing) -> Strength b score
                 (Nothing, Just score) -> Strength b score
                 (Nothing, Nothing)    -> Strength b 0



----------------------------------------------------------------------------------------------------

-- build a pseudo-grid (@len@ x @len@) of points given the total number of points total

initialPositioning :: Float -> Int -> [Point2D]
initialPositioning len numPts =
    let rt = sqrt $ fromIntegral numPts
        rndUp = ceiling rt
        rndDwn = floor rt
    in L.map Point2D $ L.concat $ case rndUp == rndDwn of
                                      True  -> coordsPerfectSq len rndUp
                                      False -> coordsMismatch numPts len rndDwn rndUp


----------------------------------------------------------------------------------------------------

-- construct the pseudo-grid when an integer square can be taken of the number of points

coordsPerfectSq :: Float -> Int -> [[(Float, Float)]]
coordsPerfectSq len bound =
    let mod = len / (fromIntegral bound)
        coords = take bound [ mod * i | i <- [0..] ]
    in L.map (L.zip coords . L.repeat) coords


----------------------------------------------------------------------------------------------------

-- construct the pseudo-grid when an integer square cannot be taken of then number of points

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




----------------------------------------------------------------------------------------------------

-- Visualization Out

----------------------------------------------------------------------------------------------------

--

reportNewPositions :: Chan (Table Int Point2D) -> Table Int Point2D -> IO ()
reportNewPositions chan tbl = writeChan chan tbl


----------------------------------------------------------------------------------------------------

--

waitVisualizationReq :: Chan (Table Int Point2D) -> IO ()
waitVisualizationReq chan = do
    addrinfo <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "8001")
    servaddr <- return $ head addrinfo
    sock <- socket (addrFamily servaddr) Stream defaultProtocol
    bindSocket sock (addrAddress servaddr)
    listen sock 10
    visualize chan sock


visualize :: Chan (Table Int Point2D) -> Socket -> IO ()
visualize chan sock = do
    verts <- readChan chan
    listv <- M.toList verts
    (connSock, cliAddr) <- accept sock
    h <- socketToHandle connSock ReadWriteMode
    hSetBinaryMode h True
    mapM_ (writePoint h) listv
    hClose h
    visualize chan sock


writePoint :: Handle -> (Int, Point2D) -> IO ()
writePoint h (id, Point2D (a, b)) =
    writeBytes 4 h ((fromIntegral id) :: Int32) >>
    writeBytes 4 h ((truncate a) :: Int32) >>
    writeBytes 4 h ((truncate b) :: Int32)

-- Writes an object @obj@ of size @nBytes@ to the file handle @h@

writeBytes :: (Storable a) => Int -> Handle -> a -> IO ()
writeBytes nBytes h obj =
    mallocBytes nBytes >>= \ptr -> poke ptr obj >> hPutBuf h ptr nBytes >> free ptr





