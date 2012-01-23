module Text.Nouns.Graph (
    textToGraph
,   representGraph
,   buildEdgeCache
,   buildVertexCache
,   graphToTulip
,   graphToTulip'
,   initializeSpringCluster
,   noScoreFilter
,   scLessThan
,   scGreaterThan
,   withinNDeviations
,   allScores
) where


import Data.Maybe
import qualified Data.HashTable.IO as M
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Database.HDBC
import Database.HDBC.PostgreSQL
import Text.Nouns
import Text.Nouns.Visualization
import Entry
import Feed
import Viterbi


----------------------------------------------------------------------------------------------------

-- Document-Graph conversion

----------------------------------------------------------------------------------------------------

-- coordinate the process of converting text documents into undirected-graphs as detailed in
-- notes

textToGraph :: IO ()
textToGraph = do

    -- gather core data
    ncores <- askNumberCores
    cores <- return $ ncores - 1

    -- spawn worker threads
    (chanV, chanE, exitC) <- spawnWorkers cores

    -- cache database
    verts <- buildVertexCache
    edges <- buildEdgeCache

    -- assimilate worker results
    pullWorkerData verts edges cores 0 chanV chanE exitC
    commitWorkerData verts edges


----------------------------------------------------------------------------------------------------

-- obtain user input regarding the number of cores

askNumberCores :: IO Int
askNumberCores = do
    putStrLn ">> How many cores should this process run on?"
    getLine >>= return . read


----------------------------------------------------------------------------------------------------

-- cache any existing database vertex records

buildVertexCache :: IO VertexCache
buildVertexCache = do
    rows <- wrap selectAll
    table <- M.new
    mapM_ (\(id, noun, count) -> M.insert table (B.pack noun) $ VData id count Update) rows
    return table

    where selectAll :: Connection -> IO [(Int, String, Int)]
          selectAll con = do
              let sel = "select * from vertex;"
                  deSql (a, b, c) = (fromSql a, fromSql b, fromSql c)
              rows <- quickQuery' con sel []
              return $ L.map (\r -> deSql (r !! 0, r !! 1, r !! 2)) rows


----------------------------------------------------------------------------------------------------

-- cache any existing database edge records

buildEdgeCache :: IO EdgeCache
buildEdgeCache = do
    _rows <- wrap selectAll
    rows <- wrap (convByteString _rows)
    outer <- M.new
    mapM_ (\(id, vtxa, vtxb, score) -> insertEdge outer (vtxa, vtxb) $ EData id score Update) rows
    return outer

    where selectAll :: Connection -> IO [(Int, Int, Int, Int)]
          selectAll con = do
              let sel = "select * from edge;"
                  deSql (a, b, c, d) = (fromSql a, fromSql b, fromSql c, fromSql d)
              rows <- quickQuery' con sel []
              return $ L.map (\r -> deSql (r !! 0, r !! 1, r !! 2, r !! 3)) rows

          convByteString :: [(Int, Int, Int, Int)] -> Connection -> IO [(Int, ByteString, ByteString, Int)]
          convByteString raw con = do
              mapM (\(id, a, b, sc) -> toNoun con (a, b) >>= \(na, nb) -> return (id, na, nb, sc)) raw

          toNoun :: Connection -> (Int, Int) -> IO (ByteString, ByteString)
          toNoun con (a, b) =
              let sel x = "select noun from vertex where id=" ++ (show x) ++ ";"
                  runQ x = quickQuery' con (sel x) [] >>= return . fromSql . flip (!!) 0 . flip (!!) 0
              in runQ a >>= \ba -> runQ b >>= \bb -> return (ba, bb)

----------------------------------------------------------------------------------------------------

-- spawn threads to process articles

spawnWorkers :: Int -> IO (TChan VFunc, TChan EFunc, TChan Bool)
spawnWorkers cores = do

    -- create comm channels
    chanV <- atomically $ newTChan
    chanE <- atomically $ newTChan
    exitC <- atomically $ newTChan

    -- train viterbi
    vit <- trainVit

    -- load & format feeds
    feeds' <- liftM fromJust allFeeds
    gpSize <- return (ceiling $ (fromIntegral (L.length feeds')) / (fromIntegral cores))
    feeds  <- return $ breakList cores gpSize feeds'

    -- perform spawn & complete
    mapM_ (\l -> forkIO $ feedsToGraph chanV chanE exitC vit l) feeds
    return (chanV, chanE, exitC)


----------------------------------------------------------------------------------------------------

-- recursively process all data discovered by worker threads until all workers have exited.

pullWorkerData :: VertexCache -> EdgeCache -> Int -> Int -> TChan VFunc -> TChan EFunc -> TChan Bool
                 -> IO ()
pullWorkerData verts edges nworkers nexit vchan echan exit
    | nexit == nworkers = return ()
    | otherwise = do
        boolVChan <- atomically $ isEmptyTChan vchan
        boolEChan <- atomically $ isEmptyTChan echan
        exitRec'd <- atomically $ isEmptyTChan exit
        verts <- case boolVChan of
                    True  -> return verts
                    False -> clearVChan verts vchan
        edges <- case boolEChan of
                    True  -> return edges
                    False -> clearEChan verts edges echan
        nexit <- case exitRec'd of
                     True  -> return nexit
                     False -> clearExitChan exit nexit
        pullWorkerData verts edges nworkers nexit vchan echan exit


clearVChan :: VertexCache -> TChan VFunc -> IO (VertexCache)
clearVChan verts chan = do
    bool <- atomically $ isEmptyTChan chan
    case bool of
        True  -> return verts
        False -> atomically (readTChan chan) >>= \f -> (runV f) verts >> clearVChan verts chan


clearEChan :: VertexCache -> EdgeCache -> TChan EFunc -> IO (EdgeCache)
clearEChan verts edges chan = do
    bool <- atomically $ isEmptyTChan chan
    case bool of
        True  -> return edges
        False -> atomically (readTChan chan) >>= \f -> (runE f) edges >> clearEChan verts edges chan


clearExitChan :: TChan Bool -> Int -> IO Int
clearExitChan chan nexit = do
    bool <- atomically $ isEmptyTChan chan
    case bool of
        True  -> return nexit
        False -> atomically (readTChan chan) >> return (nexit + 1) >>= clearExitChan chan


commitWorkerData :: VertexCache -> EdgeCache -> IO ()
commitWorkerData vcache ecache =
    wrap (\c -> M.toList vcache >>= mapM_ (commitVertexCache c) >>
               M.toList ecache >>= mapM_ (commitEdgeCache c)
         )


commitVertexCache :: Connection -> (ByteString, VertexData) -> IO (Integer)
commitVertexCache con (noun, vdata) =
    case vstatus vdata of
        Create -> do
            let sql = "insert into vertex (noun, count) values ('"
                      ++ (show noun) ++
                      "', "
                      ++ (show $ vcount vdata) ++
                      ");"
            withTransaction con (\c -> prepare c sql >>= flip execute [])
        Update -> do
            let sql = "update vertex set count="
                      ++ (show $ vcount vdata) ++
                      " where id="
                      ++ (show $ vid vdata) ++
                      ";"
            withTransaction con (\c -> prepare c sql >>= flip execute [])


commitEdgeCache :: Connection -> (ByteString, Table ByteString EdgeData) -> IO ()
commitEdgeCache con (vtxa, tbl) = M.toList tbl >>= mapM_ (commitEdge con vtxa)



commitEdge :: Connection -> ByteString -> (ByteString, EdgeData) -> IO (Integer)
commitEdge con vtxa (vtxb, edata) =
    case estatus edata of
        Update -> do
            let sql = "update edge set score="
                      ++ (show $ escore edata) ++
                      " where id="
                      ++ (show $ eid edata) ++
                      ";"
            withTransaction con (\c -> prepare c sql >>= flip execute [])
        Create -> do
            (ida, idb) <- fromNoun con (vtxa, vtxb)
            let sql = "insert into edge (nodea, nodeb, score) values ("
                      ++ (show ida) ++ ", " ++ (show idb) ++ ", "
                      ++ (show $ escore edata) ++ ");"
            withTransaction con (\c -> prepare c sql >>= flip execute [])


    where fromNoun :: Connection -> (ByteString, ByteString) -> IO (Int, Int)
          fromNoun con (a, b) =
              let sel x = "select id from vertex where noun='" ++ (show x) ++ "';"
                  runQ x = quickQuery' con (sel x) [] >>= return . fromSql . flip (!!) 0 . flip (!!) 0
              in runQ a >>= \ia -> runQ b >>= \ib -> return (ia, ib)


----------------------------------------------------------------------------------------------------

-- record the number of each noun in a list of feeds, transmit these to the aggregator thread
-- record the relationships between each noun in these feeds, also transmitting this data to
-- the aggregator thread

feedsToGraph :: TChan VFunc -> TChan EFunc -> TChan Bool -> Vit -> [Feed] -> IO ()
feedsToGraph vchan echan exit vit feeds = do
    ents <- liftM L.concat $ mapM (entries . _id) feeds
    mapM_ ((entryToGraph vchan echan vit) . B.pack . description) ents
    atomically $ writeTChan exit True


----------------------------------------------------------------------------------------------------

-- process nouns in an individual entry

entryToGraph :: TChan VFunc -> TChan EFunc -> Vit -> ByteString -> IO ()
entryToGraph vchan echan vit doc = do
    tagd <- withFilter (tag vit) doc >>= nounsAndIndices
    tlen <- return $ L.length tagd
    return (L.map coilVFunc tagd) >>= mapM_ (atomically . writeTChan vchan)
    return (integrate [] tagd 0 tlen) >>= return . L.map coilEFunc >>= mapM_ (atomically . writeTChan echan)


----------------------------------------------------------------------------------------------------

-- partially apply a function with noun information such that it will fire when the VertexCache
-- is in scope and can be applied

coilVFunc :: (ByteString, Int) -> VFunc
coilVFunc (noun, _) = VFunc (\tbl -> incCount tbl noun 1)


----------------------------------------------------------------------------------------------------

-- partially apply a function with noun relations such that it will fire, recording all discoveries
-- when the EdgeCache is in scope and can be applied

coilEFunc :: (ByteString, ByteString, Int) -> EFunc
coilEFunc (vtxa, vtxb, sc) = EFunc $ (\tbl -> incScore tbl vtxa vtxb sc)


----------------------------------------------------------------------------------------------------

-- calculates all noun-relations & scores in a document. Returns this information in the format:
-- (nounA, nounB, relationScore)

integrate :: Eq a => [(a, a, Int)] -> [(a, Int)] -> Int -> Int -> [(a, a, Int)]
integrate summary [] count ttl = summary
integrate summary [x] count ttl = summary
integrate summary (x:xs) count ttl =
    let relsByNoun = L.foldl' (integrate' x) [] $ take 100 xs
        relsSoFar = relsByNoun ++ summary
    in integrate relsSoFar xs (count + 1) ttl

    where integrate' :: Eq a => (a, Int) -> [(a, a, Int)] -> (a, Int) -> [(a, a, Int)]
          integrate' (cur, ci) summ (fut, fi) =
              case cur == fut of
                  True  -> summ
                  False -> case (fi - ci) <= 10 of
                              True  -> [(cur, fut, 3)] ++ summ
                              False -> [(cur, fut, 1)]


----------------------------------------------------------------------------------------------------

-- break a list into a set number of sublists of at most @grpSize@ in length

breakList :: Int -> Int -> [a] -> [[a]]
breakList num grpSize list = func num grpSize [] list

    where func :: Int -> Int -> [[a]] -> [a] -> [[a]]
          func 0 _ head' _ = head'
          func _ _ acc [] = acc
          func num grpSize acc xs =
              let (a, b) = L.splitAt grpSize xs
              in func (num - 1) grpSize ([a] ++ acc) b


----------------------------------------------------------------------------------------------------

-- increment count data in the VertexCache for the given noun by the given amount

incCount :: VertexCache -> ByteString -> Int -> IO ()
incCount cache noun incBy =
    cache `M.lookup` noun >>= \exists ->
    case exists of
        Just vdata -> M.insert cache noun $ vdata `setVCount` ((vcount vdata) + incBy)
        Nothing    -> M.insert cache noun $ VData (-1) incBy Create


----------------------------------------------------------------------------------------------------

-- set the vcount field of a VertexData data type

setVCount :: VertexData -> Int -> VertexData
setVCount vdata i = VData (vid vdata) i (vstatus vdata)


----------------------------------------------------------------------------------------------------

-- increase/initialize the score for the noun-relation between @vtxa@ and @vtxb@ by @incBy@

incScore :: EdgeCache -> ByteString -> ByteString -> Int -> IO ()
incScore cache vtxa vtxb incBy =
    locateEdge cache vtxa vtxb >>= \exists ->
    case exists of
        Just (edata, pos) -> updateEdge cache pos $ edata `setEScore` ((escore edata) + incBy)
        Nothing           -> insertEdge cache (vtxa, vtxb) $ EData (-1) incBy Create


----------------------------------------------------------------------------------------------------

-- set the escore field of the EdgeData datatype

setEScore :: EdgeData -> Int -> EdgeData
setEScore edata i = EData (eid edata) i (estatus edata)


----------------------------------------------------------------------------------------------------

-- locate the EdgeData describing the relation between the given vertices. If it does not exist,
-- this function evaluates to Nothing

locateEdge :: EdgeCache -> ByteString -> ByteString -> IO (Maybe (EdgeData, (ByteString, ByteString)))
locateEdge cache vtxa vtxb = do
    try1 <- attempt cache vtxa vtxb
    try2 <- attempt cache vtxb vtxa
    return $ case (try1, try2) of
                 (Nothing, Just edge) -> Just (edge, (vtxb, vtxa))
                 (Just edge, Nothing) -> Just (edge, (vtxa, vtxb))
                 (Nothing, Nothing)   -> Nothing

    where attempt :: EdgeCache -> ByteString -> ByteString -> IO (Maybe EdgeData)
          attempt cache a b = cache `M.lookup` a >>= \res ->
                              case res of
                                  Just cache' -> cache' `M.lookup` b
                                  _           -> return Nothing


----------------------------------------------------------------------------------------------------

-- update the EdgeData for the given relation in @cache@

updateEdge :: EdgeCache -> (ByteString, ByteString) -> EdgeData -> IO ()
updateEdge cache (vtxa, vtxb) edata =
    cache `M.lookup` vtxa >>= \cache' -> M.insert (fromJust cache') vtxb edata


----------------------------------------------------------------------------------------------------

-- insert the given relation + EdgeData into @cache@

insertEdge :: EdgeCache -> (ByteString, ByteString) -> EdgeData -> IO ()
insertEdge cache (vtxa, vtxb) edata =
    cache `M.lookup` vtxa >>= \exists ->
    case exists of
        Just cache' -> M.insert cache' vtxb edata
        Nothing     -> do
            inner <- M.new
            M.insert inner vtxb edata
            M.insert cache vtxa inner




----------------------------------------------------------------------------------------------------

-- Simple Force Cluster, Representation of Graph

----------------------------------------------------------------------------------------------------

-- coordinate building a haskell representation of the noun-graph

representGraph :: String -> IO (Float, Vertices, Edges)
representGraph isUser = do

    -- collect necessary information
    numPts <- numVertices
    dimen <- case isUser of
                 "user" -> askDimenSize
                 "comp" -> return 1000
    let pts = initialPositioning dimen numPts
    ids <- allVertexIds

    -- build representations
    verts <- representVertices pts ids
    edges <- representEdges ids

    return (dimen, verts, edges)


---------------------------------------------------------------------------------------------------

-- create tulip data for visualization

graphToTulip :: IO ()
graphToTulip = do

    -- background information
    numPts <- numVertices
    ids <- allVertexIds

    -- data structures
    vc <- buildVertexCache
    ec <- representEdges ids

    -- generate csv
    graphToTulipCSV vc ec


graphToTulip' :: IO ()
graphToTulip' = do
    vc <- buildVertexCache
    ec <- buildEdgeCache
    graphToTulipCSVDesc vc ec


----------------------------------------------------------------------------------------------------

-- ask user for perferred dimension size

askDimenSize :: IO Float
askDimenSize = do
    putStrLn ">> Enter a figure for the cluster-space size."
    getLine >>= return . read


----------------------------------------------------------------------------------------------------

-- number of vertices in the graph

numVertices :: IO Int
numVertices = let sel = "select count(id) from vertex;"
              in wrap (\c -> quickQuery' c sel [] >>= return . fromSql . flip (!!) 0 . L.concat)


----------------------------------------------------------------------------------------------------

-- the id of each vertex in the database-graph

allVertexIds :: IO [Int]
allVertexIds = let sel = "select id from vertex;"
               in wrap (\c -> quickQuery' c sel [] >>= return . L.map fromSql . L.concat)


----------------------------------------------------------------------------------------------------

-- list of all edges in the database-graph

allEdges :: IO [(Int, Int, Int)]
allEdges = do
    let sel = "select * from edge;"
        deSql (a, b, c) = (fromSql a, fromSql b, fromSql c)
    rows <- wrap (\c -> quickQuery' c sel [])
    return $ L.map (\row -> deSql (row !! 1, row !! 2, row !! 3)) rows


----------------------------------------------------------------------------------------------------

-- construct haskell representation of graph vertices

representVertices :: [Point2D] -> [Int] -> IO Vertices
representVertices pts ids = M.fromList $ L.zip ids pts


----------------------------------------------------------------------------------------------------

-- contruct haskell representation of graph edges

representEdges :: [Int] -> IO Edges
representEdges ids = do
    tbl <- M.new
    mapM_ (representEdge tbl ids) ids
    return tbl

representEdge :: Edges -> [Int] -> Int -> IO ()
representEdge tbl allIds id = do
    let sela = "select nodeb,score from edge where nodea=" ++ (show id) ++ ";"
        selb = "select nodea,score from edge where nodeb=" ++ (show id) ++ ";"
        toStren lst = Strength (fromSql $ lst !! 0) (fromSql $ lst !! 1)
    res <- wrap (\c -> quickQuery' c sela [] >>= \ra -> quickQuery' c selb [] >>= \rb -> return (ra, rb))
    rel <- return ((fst res) ++ (snd res)) >>= return . L.map toStren
    let blacklist = L.map target rel
        unrel = L.filter (\x -> not (x `L.elem` blacklist)) allIds
    M.insert tbl id $ Summary rel unrel


----------------------------------------------------------------------------------------------------

-- discovers all vertices not related to the target (@fr@) by subtracting the list of all vertices
-- (@ids@) from the list of those with a relation to the target (@summ@)

completeEdge :: [Int] -> (Int, RelationSummary) -> IO RelationSummary
completeEdge ids (fr, summ) = do
    let blacklist = L.map target $ related summ  -- disregard strengths
    unrelated <- return $ L.filter (\x -> not (x `L.elem` blacklist)) ids
    return $ Summary (related summ) unrelated


----------------------------------------------------------------------------------------------------

-- build a pseudo-grid (@len@ x @len@) of points given the total number of points total

initialPositioning :: Float -> Int -> [Point2D]
initialPositioning len numPts =
    let rt = sqrt $ fromIntegral numPts
        rndUp = ceiling rt
        rndDwn = floor rt
    in L.concat $ case rndUp == rndDwn of
                      True  -> coordsPerfectSq len rndUp
                      False -> coordsMismatch numPts len rndDwn rndUp


----------------------------------------------------------------------------------------------------

-- construct the pseudo-grid when an integer square can be taken of the number of points

coordsPerfectSq :: Float -> Int -> [[Point2D]]
coordsPerfectSq len bound =
    let mod = len / (fromIntegral bound)
        coords = take bound [ mod * i | i <- [0..] ]
    in L.map (L.zip coords . L.repeat) coords


----------------------------------------------------------------------------------------------------

-- construct the pseudo-grid when an integer square cannot be taken of then number of points

coordsMismatch :: Int -> Float -> Int -> Int -> [[Point2D]]
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

-- Spring Cluster, Representation of graph

----------------------------------------------------------------------------------------------------


initializeSpringCluster :: SpringClusterConfiguration -> IO (Edges, Vertices3D)
initializeSpringCluster scc = do
    scores <- allScores >>= return . L.map fromIntegral
    es <- selectEdges (_selFunc scc) scores
    vs <- selectVerts (_dimen scc)
    return (es, vs)


selectEdges :: ([Float] -> Int -> [String]) -> [Float] -> IO Edges
selectEdges func scores = do
    tbl <- M.new
    ids <- allVertexIds
    mapM_ (selectProxy tbl ids) ids
    return tbl

    where selectProxy tbl ids id = let sels = func scores id
                                       sela = sels !! 0
                                       selb = sels !! 1
                                   in selectEdge tbl ids sela selb id


selectEdge :: Edges -> [Int] -> String -> String -> Int -> IO ()
selectEdge tbl allIds sela selb id = do
    let toStren lst = Strength (fromSql $ lst !! 0) (fromSql $ lst !! 1)
    res <- wrap (\c -> quickQuery' c sela [] >>= \ra -> quickQuery' c selb [] >>= \rb -> return (ra, rb))
    rel <- return ((fst res) ++ (snd res)) >>= return . L.map toStren
    let blacklist = L.map target rel
        unrel = L.filter (\x -> not (x `L.elem` blacklist)) allIds
    M.insert tbl id $ Summary rel unrel


selectVerts :: Int -> IO Vertices3D
selectVerts dimen = do
    nverts <- numVertices
    ids <- allVertexIds
    initialPositioning3D ids dimen nverts


noScoreFilter :: [Float] -> Int -> [String]
noScoreFilter _ id = [("select nodeb,score from edge where nodea=" ++ (show id) ++ ";"),
                      ("select nodea,score from edge where nodeb=" ++ (show id) ++ ";")]


scLessThan :: Int -> [Float] -> Int -> [String]
scLessThan i _ id =
    [("select nodeb,score from edge where id=" ++ (show id) ++ " and score<" ++ (show i) ++ ";"),
     ("select nodea,score from edge where id=" ++ (show id) ++ " and score<" ++ (show i) ++ ";")]


scGreaterThan :: Int -> [Float] -> Int -> [String]
scGreaterThan i _ id =
    [("select nodeb,score from edge where id=" ++ (show id) ++ " and score>" ++ (show i) ++ ";"),
     ("select nodea,score from edge where id=" ++ (show id) ++ " and score>" ++ (show i) ++ ";")]


withinNDeviations :: Int -> [Float] -> Int -> [String]
withinNDeviations n lst id =
    let floatn = fromIntegral n
        stddev = stdDeviation lst
        avg = (L.foldl' (+) 0 lst) / (fromIntegral $ L.length lst)
        upBound = floor $ avg + ((fromIntegral n) * stddev)
        dnBound = ceiling $ avg - ((fromIntegral n) * stddev)
    in [("select nodeb,score from edge where id=" ++ (show id) ++ "score<"
        ++ (show upBound) ++ " and score>" ++ (show dnBound) ++ ";")
        ,
        ("select nodea,score from edge where id=" ++ (show id) ++ "score<"
        ++ (show upBound) ++ " and score>" ++ (show dnBound) ++ ";")]


stdDeviation :: [Float] -> Float
stdDeviation vals =
    let nvals = fromIntegral (L.length vals)
        avg = (L.foldl' (+) 0 vals) / nvals
        sqDiffs = L.map (\x -> (abs (x - avg))^2) vals
        avgDiff = (L.foldl' (+) 0 sqDiffs) / nvals
    in sqrt avgDiff


allScores :: IO [Int]
allScores = do
    let sel = "select score from edge;"
    rows <- wrap (\c -> quickQuery' c sel [])
    return $ L.map (fromSql . flip (!!) 0) rows


initialPositioning3D :: [Int] -> Int -> Int -> IO Vertices3D
initialPositioning3D ids dimen npts =
    let cbrt = (ceiling (fromIntegral npts ** (1/3)))
        interval = (fromIntegral dimen) / (fromIntegral cbrt)
        vals = take cbrt [ interval * i | i <- [0..] ]
        coords = L.concat $ L.concat $ L.map (\x -> L.map (\y -> L.map (\z -> (x, y, z)) vals) vals) vals
    in M.new >>= \tbl -> mapM_ (\(a, b) -> M.insert tbl a b) (L.zip ids coords) >> return tbl


