module Text.Nouns.Visualization (
    waitVisualizationReq
,   graphToTulipCSV
,   graphToTulipCSVDesc
) where


import Data.Int
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.HashTable.IO as M
import Control.Concurrent
import Control.Concurrent.Chan
import System.IO
import Foreign.Storable
import Foreign.Marshal.Alloc
import Network.Socket
import Text.Nouns


----------------------------------------------------------------------------------------------------

-- Visualization: Force-based cluster, Browser display

----------------------------------------------------------------------------------------------------


--

waitVisualizationReq :: Chan (Table Int Point2D) -> IO ()
waitVisualizationReq chan = do
    addrinfo <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "8000")
    servaddr <- return $ head addrinfo
    sock <- socket (addrFamily servaddr) Stream defaultProtocol
    bindSocket sock (addrAddress servaddr)
    listen sock 10
    serve chan sock


----------------------------------------------------------------------------------------------------

--

serve :: Chan (Table Int Point2D) -> Socket -> IO ()
serve chan sock = do
    (connSock, cliAddr) <- accept sock
    forkIO (visualize chan connSock)
    serve chan sock


----------------------------------------------------------------------------------------------------

--

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


----------------------------------------------------------------------------------------------------

--

writePoint :: Handle -> (Int, Point2D) -> IO ()
writePoint h (id, (a, b)) =
    writeBytes 4 h ((fromIntegral id) :: Int32) >>
    writeBytes 4 h ((truncate a) :: Int32) >>
    writeBytes 4 h ((truncate b) :: Int32)


----------------------------------------------------------------------------------------------------

-- Writes an object @obj@ of size @nBytes@ to the file handle @h@

writeBytes :: (Storable a) => Int -> Handle -> a -> IO ()
writeBytes nBytes h obj =
    mallocBytes nBytes >>= \ptr -> poke ptr obj >> hPutBuf h ptr nBytes >> free ptr




----------------------------------------------------------------------------------------------------

-- Visualization: Direct graph visualization, Tulip display (CSV-out)

----------------------------------------------------------------------------------------------------

-- generate csv files for ID labeled graph

graphToTulipCSV :: VertexCache -> Edges -> IO ()
graphToTulipCSV vc ec = do
    let vcsvSeed = "node_id;count\n"

    vcsv <- M.toList vc >>= \lstVC -> return $
           L.foldl' (\acc (k, v) -> acc ++ (show $ vid v) ++ ";" ++ (show $ vcount v) ++ "\n")
               vcsvSeed
               lstVC
    ecsv <- M.toList ec >>= return . entryCSV "\"source\",\"target\"\n"

    writeFile "./data/vertex.csv" vcsv
    writeFile "./data/edge.csv" ecsv

    putStrLn ">> CSV Generated."


-- generate csv files for Descriptive labeled graph

graphToTulipCSVDesc :: VertexCache -> EdgeCache -> IO ()
graphToTulipCSVDesc vc ec = do
    let vcsvSeed = "node_id;count\n"
    vcsv <- M.toList vc >>= \lstVC -> return $
           L.foldl' (\acc (k, v) -> acc ++ (show $ vid v) ++ ";" ++ (show $ vcount v) ++ "\n")
               vcsvSeed
               lstVC

    ecsv <- M.toList ec >>= entryCSV' "\"source\",\"target\"\n"

    writeFile "./data/vertex.csv" vcsv
    writeFile "./data/edge.csv" ecsv

    putStrLn ">> CSV Generated."



entryCSV :: String -> [(Int, RelationSummary)] -> String

entryCSV acc [] = ""

entryCSV acc [x] =
    let components = L.map (\tar -> (show $ fst x) ++ "," ++ (show $ target tar) ++ "\n") $ related $ snd x
        concaten'd = L.foldl' (++) "" components
    in acc ++ concaten'd

entryCSV acc (x:xs) =
    let components = L.map (\tar -> (show $ fst x) ++ "," ++ (show $ target tar) ++ "\n") $ related $ snd x
        concaten'd = L.foldl' (++) "" components
    in entryCSV (acc ++ concaten'd) xs



entryCSV' :: String -> [(ByteString, Table ByteString EdgeData)] -> IO String

entryCSV' acc [] = return ""

entryCSV' acc [x] = do
    components <- M.toList (snd x) >>= return . L.map (\tar -> (show $ fst x) ++ "," ++ (show $ fst tar) ++ "\n")
    let concaten'd = L.foldl' (++) "" components
    return (acc ++ concaten'd)

entryCSV' acc (x:xs) = do
    components <- M.toList (snd x) >>= return . L.map (\tar -> (show $ fst x) ++ "," ++ (show $ fst tar) ++ "\n")
    let concaten'd = L.foldl' (++) "" components
    return (acc ++ concaten'd) >>= flip entryCSV' xs


