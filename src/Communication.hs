module Communication (
    communicate
) where


import Data.Maybe
import Data.Int
import qualified Data.List as L
import qualified Data.ByteString as B
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import System.IO
import Network.Socket
import Foreign.Storable
import Foreign.Marshal.Alloc
import Parse


----------------------------------------------------------------------------------------------------

-- type aliases

type ByteString = B.ByteString


----------------------------------------------------------------------------------------------------

-- data type to represent different package formats

data PackageType = UrlTranslation
                 | FeedSubmission
                 | FeedContentRequest
                 | ClusterNouns


----------------------------------------------------------------------------------------------------

-- data type to contain data sent via socket

data Package = PreScript { lengthInChars :: Int32 }
             | Message   { url           :: String }
             | Response  { response      :: ByteString }
    deriving Show


----------------------------------------------------------------------------------------------------

-- special character to separate lists sent via tcp socket

listSeparator :: Char
listSeparator = '|'


----------------------------------------------------------------------------------------------------

-- given an 8-bit Integer code, determine the type of package being sent

decodePackageType :: Int8 -> PackageType
decodePackageType i = case i of
                          1 -> UrlTranslation
                          2 -> FeedSubmission
                          3 -> FeedContentRequest


----------------------------------------------------------------------------------------------------

-- handle boilerplate required for socket set-up. Start listening on port 8001

communicate :: IO ()
communicate = do
    addrinfo <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "8001")
    servaddr <- return $ head addrinfo
    sock <- socket (addrFamily servaddr) Stream defaultProtocol
    bindSocket sock (addrAddress servaddr)
    listen sock 10
    serve sock


----------------------------------------------------------------------------------------------------

-- listen for connections to the socket, hand them off to a new *green* thread

serve :: Socket -> IO ()
serve sock = do
    (connSock, cliAddr) <- accept sock
    forkIO $ handleClient connSock
    serve sock


----------------------------------------------------------------------------------------------------

-- determine package type, read package, delegate processing of package based on package
-- type/contents

handleClient :: Socket -> IO ()
handleClient sock = do
    h <- socketToHandle sock ReadWriteMode
    hSetBinaryMode h True
    pt <- readBytes 1 h >>= return . decodePackageType
    pack <- readPackage h
    case pt of
        UrlTranslation -> putStrLn (show pack) >> translateURL pack >>= respond h >> hClose h



----------------------------------------------------------------------------------------------------

-- read a package from a socket (represented as a Handle)

readPackage :: Handle -> IO [Package]
readPackage h = readBytes 4 h >>= \a -> replicateM a (readBytes 1 h) >>= \b ->
                return [PreScript (fromIntegral a), Message b]


----------------------------------------------------------------------------------------------------

-- translate a url into its compent links using the link parser (Parse module), re-package for
-- response

translateURL :: [Package] -> IO [Package]
translateURL p = htmlLinks (url $ p !! 1) >>= \res ->
                 case res of
                     Nothing  -> return $ [PreScript 0, Message ""]
                     Just lks -> return $ format lks

    where format :: [String] -> [Package]
          format lst = let x = join $ L.intersperse [listSeparator] lst
                           l = fromIntegral $ L.length x
                       in [PreScript l, Message x]


----------------------------------------------------------------------------------------------------

-- send a package

respond :: Handle -> [Package] -> IO ()
respond h p = do
    l <- return $ lengthInChars $ p !! 0
    putStrLn $ show l
    writeBytes 4 h l
    mapM_ (writeBytes 1 h) $ url $ p !! 1


----------------------------------------------------------------------------------------------------

-- Writes an object @obj@ of size @nBytes@ to the file handle @h@

writeBytes :: (Storable a) => Int -> Handle -> a -> IO ()
writeBytes nBytes h obj =
    mallocBytes nBytes >>= \ptr -> poke ptr obj >> hPutBuf h ptr nBytes >> free ptr


----------------------------------------------------------------------------------------------------

-- Reads an object of size @nBytes@ from the file @h@. File cursor must be set by caller

readBytes :: (Storable a) => Int -> Handle -> IO (a)
readBytes nBytes h =
    mallocBytes nBytes >>= \ptr -> hGetBuf h ptr nBytes >> peek ptr >>= \ret -> free ptr >> return ret


----------------------------------------------------------------------------------------------------

