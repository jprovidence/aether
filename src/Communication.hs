module Communication (

) where


import Data.Maybe
import Data.Int
import qualified Data.ByteString as B
import Control.Concurrent.MVar
import Control.Concurrent
import System.IO
import Network.Socket


type ByteString = B.ByteString

data PackageType = UrlTranslation
                 | FeedSubmission
                 | FeedContentRequest


data Package = PreScript { packageType :: Int8 }
             | Message   { url         :: String }
             | Response  { response    :: ByteString }


endOfMessage :: Char
endOfMessage = '\n'

listSeparator :: Char
listSeparator = '|'

decodePackageType :: Int8 -> PackageType
decodePackageType i = case i of
                          1 -> UrlTranslation
                          2 -> FeedSubmission
                          3 -> FeedContentRequest


communicate :: IO ()
communicate = do
    addrinfo <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "8001")
    servaddr <- return $ head addrinfo
    sock <- socket (addrFamily servaddr) Stream defaultProtocol
    bindSocket sock (addrAddress servaddr)
    listen sock 10
    serve sock


serve :: Socket -> IO ()
serve sock = do
    (connSock, cliAddr) <- accept sock
    forkIO $ handleClient connSock
    serve sock


handleClient :: Socket -> IO ()
handleClient sock = do
    h <- socketToHandle sock ReadWriteMode
    return ()

