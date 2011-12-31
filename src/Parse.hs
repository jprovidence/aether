module Parse (
    urlEntries
,   htmlLinks
,   parseFeed
) where

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import Data.Char
import Data.List
import Control.Monad
import qualified Network.HTTP as H
import Network.URI
import Text.XML.HXT.Core
import Entry


userAgent :: String
userAgent = "Mozilla/5.0 (en-US) Firefox/2.0.0.6667"


----------------------------------------------------------------------------------------------------

-- send Get request for HTML, and extract all links (<a href="xxx"> </a>)

htmlLinks :: String -> IO (Maybe [String])
htmlLinks url =
    get url >>= \r ->
    case r of
        Nothing   -> return Nothing
        Just body -> parseLinks body >>= return . Just


----------------------------------------------------------------------------------------------------

-- Given an RSS/Atom feed url, extract information from each '<entry>'/'<item>'

urlEntries :: String -> IO (Maybe [Entry])
urlEntries url =
    get url >>= \r ->
    case r of
        Nothing   -> return Nothing
        Just body -> parseFeed body >>= return . Just


----------------------------------------------------------------------------------------------------

-- 'runs' the arrow that extracts all links from HTML

parseLinks :: String -> IO [String]
parseLinks body = runX (readHtml body >>> linkify) >>= return . (flip (!!) 0)


----------------------------------------------------------------------------------------------------

-- 'runs' the arrow that extracts Entry data-types from XML

parseFeed :: String -> IO [Entry]
parseFeed feed = runX (readXml feed >>> entrify) >>= return . (flip (!!) 0)


----------------------------------------------------------------------------------------------------

-- parse the URL, delegate sending an HTTP Get request to #sendRequest once the URL is valid

get :: String -> IO (Maybe String)
get url = case parseURI url of
              Nothing  -> return Nothing
              Just uri -> sendRequest uri


----------------------------------------------------------------------------------------------------

-- send an HTTP Get request to the URI provided.

sendRequest :: URI -> IO (Maybe String)
sendRequest uri = do
    resp <- H.simpleHTTP (H.Request uri H.GET [H.Header H.HdrUserAgent userAgent] "")
    case resp of
        Left _  -> return Nothing
        Right r -> return $ Just $ H.rspBody r


----------------------------------------------------------------------------------------------------

-- parse XML string into Tree form to use with arrows

readXml :: String -> IOStateArrow s b XmlTree
readXml = readString [withValidate no]


----------------------------------------------------------------------------------------------------

-- parse HTML string into Tree form to use with arrows

readHtml :: String -> IOStateArrow s b XmlTree
readHtml = readString [withValidate no, withParseHTML yes, withWarnings no]


----------------------------------------------------------------------------------------------------

-- extract all links in a given HTML Tree

linkify :: ArrowXml a => a XmlTree [String]
linkify = proc tree -> do
    returnA <<< listA (atTagCase "a" >>> getAttrValue "href") -< tree


----------------------------------------------------------------------------------------------------

-- High level metaphor for the process of extracting entries from XML String. See getEntries for
-- actual extraction

entrify :: ArrowXml a => a XmlTree [Entry]
entrify = proc tree -> do
    returnA <<< merge <<< getEntries "item" &&& getEntries "entry" -< tree


----------------------------------------------------------------------------------------------------

-- extract Entries from XML String

getEntries :: ArrowXml a => String -> a XmlTree [Entry]
getEntries tag = proc xml -> do
    desc <- listA (atTag tag >>> atTag "description" >>> text) -< xml
    ddat <- merge <<< textAt "pubDate" &&& textAt "date" -< xml
    titl <- listA (atTag tag >>> atTag "title" >>> text) -< xml
    link <- merge <<< textAt "link" &&& textAt "origLink" -< xml
    returnA <<< mapCreate <<< arr (zip4 desc titl link) -<< ddat

    where mapCreate :: ArrowXml a => a [(String, String, String, String)] [Entry]
          mapCreate = arr (map (\(a, b, c, d) -> Entry a b c d))

          textAt :: ArrowXml a => String -> a XmlTree [String]
          textAt str = proc xml -> do
              returnA <<< listA (atTag tag >>> atTagCase str >>> text) -< xml


----------------------------------------------------------------------------------------------------

-- merge tuple of two lists into a single list.

merge :: ArrowXml a => a ([b], [b]) [b]
merge = proc tpl -> do
    returnA <<< arr (\x -> (fst x) ++ (snd x)) -< tpl


----------------------------------------------------------------------------------------------------

-- filter Tree to include only nodes that match that provided tag

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)


----------------------------------------------------------------------------------------------------

-- similar to #atTag, disregards namespaces and case

atTagCase :: ArrowXml a => String -> a XmlTree XmlTree
atTagCase tag = deep (isElem >>> hasNameWith ((== upTag) . upper . localPart))

    where upTag :: String
          upTag = upper tag

          upper :: String -> String
          upper = map toUpper


----------------------------------------------------------------------------------------------------

-- get the text at a given node

text :: ArrowXml a => a XmlTree String
text = getChildren >>> getText


----------------------------------------------------------------------------------------------------
