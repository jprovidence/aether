module Text.Nouns.Graph (
    textToGraph
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
import Entry
import Feed
import Viterbi


----------------------------------------------------------------------------------------------------

-- typedefs

type ByteString = B.ByteString
type Table k v = M.CuckooHashTable k v
type VertexCache = Table ByteString VertexData
type EdgeCache = Table ByteString (Table ByteString EdgeData)


----------------------------------------------------------------------------------------------------

-- data type to indicate whether a particular record must be updated or inserted to the database
-- upon completion of graph-conversion

data DBStatus = Update
              | Create
    deriving Show


----------------------------------------------------------------------------------------------------

-- data type to represent the state (or future state) of a given vertex in the database

data VertexData = VData { vid     :: Int
                        , vcount  :: Int
                        , vstatus :: DBStatus
                        } deriving Show


----------------------------------------------------------------------------------------------------

-- data type to represent the state (or future state) of a given edge in the database

data EdgeData = EData { eid     :: Int
                      , escore  :: Int
                      , estatus :: DBStatus
                      } deriving Show


----------------------------------------------------------------------------------------------------

-- function that will be partially applied with information regarding updates to a vertex. Will fire
-- when the VertexCache is in scope and can be applied

newtype VFunc = VFunc { runV :: (VertexCache -> IO ()) }


----------------------------------------------------------------------------------------------------

-- same as EFunc, for edge data instead. Note: all VFuncs generated during a conversion must be
-- fired before EFuncs will evaluate correctly

newtype EFunc = EFunc { runE :: (EdgeCache -> IO ()) }


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
                  deSql (a, b, d, c) = (fromSql a, fromSql b, fromSql c, fromSql d)
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
    tagd <- tag vit doc >>= nounsAndIndices
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
