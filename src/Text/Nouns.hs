module Text.Nouns (
    convertDatabase
,   runCluster
,   initialPositioning
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
import Entry
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


data ClusterConfiguration = CC { _attrF :: ForceFunc
                               , _replF :: ForceFunc
                               , _edges :: Table Int RelationCache
                               , _verts :: Table Int Point2D
                               }


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

convertDatabase :: IO ()
convertDatabase = do
    vit <- trainVit
    mvar <- newEmptyMVar
    feeds <- liftM fromJust allFeeds
    ents <- mapM (\f -> find f entries) feeds >>= return . L.concat
    docs <- return $ L.map (B.pack . description) ents
    forkIO $ (\mv -> documentsToGraph vit (odds docs) >> putMVar mv True) mvar
    documentsToGraph vit $ evens docs
    takeMVar mvar
    return ()


evens :: [a] -> [a]
evens lst = snd $ L.foldl' alternator (False, []) lst

odds :: [a] -> [a]
odds lst = snd $ L.foldl' alternator (True, []) lst

alternator :: (Bool, [a]) -> a -> (Bool, [a])
alternator acc x = case fst acc of
                       True  -> (False, [x] ++ (snd acc))
                       False -> (True, snd acc)

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
    let ins = "insert into edge (nodea, nodeb, score) values ("
              ++ (show vtxa) ++ ", "
              ++ (show vtxb) ++ ", "
              ++ (show 0) ++ ");"
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

runCluster :: String -> IO ()
runCluster src = do
    case src of
        "user" -> do
            putStrLn ">> Enter cluster-space size."
            dimen <- getLine >>= return . read
            (edges, verts) <- prepareCache dimen
            chan <- prepVisualOut
            runCluster' chan dimen src edges verts
        _ -> do
            return ()


runCluster' :: Chan (Table Int Point2D) -> Float -> String -> Table Int RelationCache ->
               Table Int Point2D -> IO ()
runCluster' chan dimen src edges verts =
    case src of
        "user" -> do
            (attrF, replF) <- userConfigureForces dimen
            config <- return $ CC attrF replF edges verts
            enterClusterCycle chan config 0
            stat <- confirmExit
            case stat of
                0 -> runCluster src
                1 -> runCluster' chan dimen src edges verts
                2 -> return ()

        _      -> do
            return ()


prepareCache :: Float -> IO (Table Int RelationCache, Table Int Point2D)
prepareCache dimen = do
    putStrLn ">> Preparing cache. Perform this in parallel? y/n"
    input <- getLine
    case affirmative input of
        True -> do
            putStrLn ">> Parallelizing."
            mvar <- newEmptyMVar
            count <- numVertices
            forkIO $ (\mv -> cacheEdges >>= putMVar mv) mvar
            verts <- cacheVertices $ initialPositioning dimen count
            edges <- takeMVar mvar
            return (edges, verts)
        False -> do
            putStrLn ">> Single-Threading."
            count <- numVertices
            verts <- cacheVertices $ initialPositioning dimen count
            edges <- cacheEdges
            return (edges, verts)


prepVisualOut :: IO (Chan (Table Int Point2D))
prepVisualOut = do
    chan <- newChan
    forkIO (waitVisualizationReq chan)
    return chan


userConfigureForces :: Float -> IO (ForceFunc, ForceFunc)
userConfigureForces dimen = do
    putStrLn ">> Enter a value for the max-move divisor."
    putStrLn (">> Note: Current cluster-space size is " ++ (show dimen) ++ ".")
    moveDivisor <- getLine >>= return . read

    moveMax <- return $ dimen / moveDivisor
    scMax <- scoreMax

    putStrLn ">> Enter a value for the repulsion threshold."
    repulThresh <- getLine >>= return . read

    modA <- buildForce "Attractive"
    modR <- buildForce "Repulsive"

    attrF <- return $ configAttractiveMod modA dimen moveMax scMax
    replF <- return $ configRepulsiveMod modR moveMax repulThresh
    return (attrF, replF)


buildForce :: String -> IO (Float -> Float -> Float)
buildForce str = do
    putStrLn (">> How would you like to modulate " ++ str ++ " Forces?")
    putStrLn ">> 1 : Greater with distance."
    putStrLn ">> 2 : Lesser with distance."
    partA <- getLine >>= return . read
    putStrLn (">> Which function type should be used to modulate " ++ str ++ " Forces?")
    putStrLn ">> 1 : Linear"
    putStrLn ">> 2 : Exponential"
    putStrLn ">> 3 : Fractinal exponential"
    partB <- getLine >>= return . read
    return $ case (partA, partB) of
                 (1, 1) -> linearGD
                 (1, 2) -> exponentialGD
                 (1, 3) -> fracExponentialGD
                 (2, 1) -> linearLD
                 (2, 2) -> exponentialLD
                 (2, 3) -> fracExponentialLD


confirmExit :: IO Int
confirmExit = do
    putStrLn ">> Enter one of the following numbers to continue."
    putStrLn ">> 0 : Run another cluster, update all cached data."
    putStrLn ">> 1 : Run another cluster, use existing cache."
    putStrLn ">> 2 : Exit"
    getLine >>= return . read


affirmative :: String -> Bool
affirmative str = (str == "y") || (str == "Y")



enterClusterCycle :: Chan (Table Int Point2D) ->
                    ClusterConfiguration     ->
                    Int                      ->
                    IO ()
enterClusterCycle chan config count =
     case count == 100 of
        True -> do
            putStrLn ">> 100 cycles completed, continue? y/n"
            input <- getLine
            case affirmative input of
                True  -> enterClusterCycle chan config 0
                False -> return ()

        False -> do
            let (e, v, a, r) = ((_edges config), (_verts config), (_attrF config), (_replF config))
            reportNewPositions chan v
            newVerts <- clusterCycle e v a r
            threadDelay 500000
            enterClusterCycle chan (CC a r e newVerts) (count + 1)


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

    in case move == 0 of
        True  -> Point2D (xb, yb)
        False -> Point2D ((move * (xa - xb)) + xb, (move * (ya - yb)) + yb)

    where pythagoras :: Float -> Float -> Float
          pythagoras a b = sqrt ((a^2) + (b^2))

          makePt :: Float -> (Float, Float) -> (Float, Float) -> Point2D
          makePt move (xa, ya) (xb, yb) =
              let (xz, yz) = ((move * (xa - xb)) + xb, (move * (ya - yb)) + yb)
              in Point2D (sanitize xz, sanitize yz)

          sanitize :: Float -> Float
          sanitize = lwrBnd . uprBnd

          uprBnd :: Float -> Float
          uprBnd x = case (x > 1000.0) of
                           False -> x
                           True  -> 1000.0

          lwrBnd :: Float -> Float
          lwrBnd x = case (x < 0.0) of
                         False -> x
                         True  -> 0.0



----------------------------------------------------------------------------------------------------

-- configure a repulsive force function for easy passing

configRepulsiveMod :: (Float -> Float -> Float) -> Float -> Float -> ForceFunc
configRepulsiveMod f moveMax thresh = ForceFunc (modRepulsiveForce f moveMax thresh)


----------------------------------------------------------------------------------------------------

-- configure an attractive force function for easy passing

configAttractiveMod :: (Float -> Float -> Float) -> Float -> Float -> Int -> ForceFunc
configAttractiveMod f dimen moveMax scoreMax =
    ForceFunc (modAttractiveForce f dimen moveMax scoreMax)


----------------------------------------------------------------------------------------------------

-- modulate the repulsive move according to a linear function

modRepulsiveForce :: (Float -> Float -> Float) -> Float -> Float -> Int -> Float -> Float
modRepulsiveForce f moveMax thresh _ dist =
    let trespass = f dist thresh
    in case dist < thresh of
           False -> 0
           True  -> trespass * moveMax


----------------------------------------------------------------------------------------------------

-- modulate the attractive force on a vertex according to distance and the chosen mod function

modAttractiveForce :: (Float -> Float -> Float) -> Float -> Float -> Int -> Int -> Float -> Float
modAttractiveForce f dimen moveMax scoreMax score dist =
    let fscore = fromIntegral score
        fscoreMax = fromIntegral scoreMax
        naiveMove = (fscore / fscoreMax) * moveMax   -- move distance before modulation
        scHypotenuse = sqrt ((dimen^2) + (dimen^2))  -- greatest dist possible
        moveMultiplier = f dist scHypotenuse         -- apply modulating function

    in naiveMove * moveMultiplier


----------------------------------------------------------------------------------------------------

-- modulating function for attractive forces, linear

linearGD :: Float -> Float -> Float
linearGD a b = a / b


linearLD :: Float -> Float -> Float
linearLD a b = 1 - (linearGD a b)


----------------------------------------------------------------------------------------------------

-- modulating function for attractive forces, exponential

exponentialGD :: Float -> Float -> Float
exponentialGD a b = (a^2) / (b^2)


exponentialLD :: Float -> Float -> Float
exponentialLD a b = 1 - (exponentialGD a b)

----------------------------------------------------------------------------------------------------

-- modulating function for repulsive forces, opposite exponential

fracExponentialGD :: Float -> Float -> Float
fracExponentialGD a b = 1 - (fracExponentialLD a b)


fracExponentialLD :: Float -> Float -> Float
fracExponentialLD a b = (sqrt a) / (sqrt b)


----------------------------------------------------------------------------------------------------

-- load edge data into memory from database

cacheEdges :: IO (Table Int RelationCache)
cacheEdges = do
    let sel = "select id from vertex;"
    allIds <- vertexIds
    htbl <- M.new
    mvar <- newEmptyMVar
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
    --putStrLn ">> Visualizations now available"
    addrinfo <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "8000")
    servaddr <- return $ head addrinfo
    sock <- socket (addrFamily servaddr) Stream defaultProtocol
    bindSocket sock (addrAddress servaddr)
    listen sock 10
    serve chan sock


serve :: Chan (Table Int Point2D) -> Socket -> IO ()
serve chan sock = do
    (connSock, cliAddr) <- accept sock
    forkIO (visualize chan connSock)
    serve chan sock


visualize :: Chan (Table Int Point2D) -> Socket -> IO ()
visualize chan sock = do
    verts <- readChan chan
    listv <- M.toList verts
    listlen <- return $ L.length listv
    h <- socketToHandle sock ReadWriteMode
    hSetBinaryMode h True
    writeBytes 4 h ((fromIntegral listlen) :: Int32)
    mapM_ (writePoint h) listv
    hClose h


writePoint :: Handle -> (Int, Point2D) -> IO ()
writePoint h (id, Point2D (a, b)) =
    writeBytes 4 h ((fromIntegral id) :: Int32) >>
    writeBytes 4 h ((truncate a) :: Int32) >>
    writeBytes 4 h ((truncate b) :: Int32)

-- Writes an object @obj@ of size @nBytes@ to the file handle @h@

writeBytes :: (Storable a) => Int -> Handle -> a -> IO ()
writeBytes nBytes h obj =
    mallocBytes nBytes >>= \ptr -> poke ptr obj >> hPutBuf h ptr nBytes >> free ptr





