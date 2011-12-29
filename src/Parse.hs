module Parse (
    listEntries
,   Entry(Entry, description, date)
) where

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import Control.Monad
import qualified Network.HTTP as H
import Network.URI
import Text.XML.HXT.Core


userAgent :: String
userAgent = "Mozilla/5.0 (en-US) Firefox/2.0.0.6667"


data Entry = Entry { description :: String
                   , date        :: String
                   } deriving Show


listEntries :: String -> IO (Maybe [Entry])
listEntries url =
    get url >>= \r ->
    case r of
        Nothing   -> return Nothing
        Just body -> parseFeed body >>= return . Just

get :: String -> IO (Maybe String)
get url = case parseURI url of
              Nothing  -> return Nothing
              Just uri -> sendRequest uri

sendRequest :: URI -> IO (Maybe String)
sendRequest uri = do
    resp <- H.simpleHTTP (H.Request uri H.GET [H.Header H.HdrUserAgent userAgent] "")
    case resp of
        Left _  -> return Nothing
        Right r -> return $ Just $ H.rspBody r

parseFeed :: String -> IO [Entry]
parseFeed feed = runX (readXml feed >>> haskellize) >>= return . (flip (!!) 0)

readXml :: String -> IOStateArrow s b XmlTree
readXml = readString [withValidate no]

haskellize :: ArrowXml a => a XmlTree [Entry]
haskellize = proc tree -> do
    returnA <<< merge <<< getEntries "item" &&& getEntries "entry" -< tree

getEntries :: ArrowXml a => String -> a XmlTree [Entry]
getEntries tag = proc xml -> do
    desc <- listA (atTag tag >>> atTag "description" >>> text) -< xml
    pubd <- listA (atTag tag >>> atTag "pubDate" >>> text) -< xml
    ddat <- listA (atTag tag >>> atTag "date" >>> text) -< xml
    returnA <<< arr (map (\x -> Entry (fst x) (snd x))) <<< arr (zip desc) <<< merge -<< (pubd, ddat)

merge :: ArrowXml a => a ([b], [b]) [b]
merge = proc tpl -> do
    returnA <<< arr (\x -> (fst x) ++ (snd x)) -< tpl

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

text :: ArrowXml a => a XmlTree String
text = getChildren >>> getText
