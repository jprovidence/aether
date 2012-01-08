module Parse (
    urlEntries
,   htmlText
,   htmlLinks
,   parseFeed
) where

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Control.Monad
import qualified Network.HTTP as H
import Network.URI
import Text.XML.HXT.Core
import Text.Regex.PCRE.Light
import Entry


type ByteString = B.ByteString

data LinkGroup = LinkGroup { outgoing   :: [String]
                           , sameDomain :: [String]
                           , ads        :: [String]
                           } deriving Show

data LinkType = Outgoing    String
              | SameDomain  String
              | Advert      String


emptyLG :: LinkGroup
emptyLG = LinkGroup [] [] []


userAgent :: String
userAgent = "Mozilla/5.0 (en-US) Firefox/2.0.0.6667"


----------------------------------------------------------------------------------------------------

-- get text from html document

htmlText :: String -> IO (Maybe String)
htmlText url =
    get url >>= \r ->
    case r of
        Nothing   -> return Nothing
        Just body -> runX (readHtml body >>> deep (getText)) >>= return . Just . join . cleanseHtml


----------------------------------------------------------------------------------------------------

-- strip html to english only (or as close as possible)

cleanseHtml :: [String] -> [String]
cleanseHtml wds = filter nonsense wds

    where nonsense :: String -> Bool
          nonsense str = foldl' checkBL True str

          checkBL :: Bool -> Char -> Bool
          checkBL b c = case b of
                            False -> b
                            True  -> not (c `elem` blacklist)

          blacklist :: [Char]
          blacklist = ['\t', '\n', '\r', '<', '>']


----------------------------------------------------------------------------------------------------

-- groups url links by whether they are incoming, outgoing or adverts

segregateLinks :: String -> IO (Maybe LinkGroup)
segregateLinks url =
    htmlLinks url >>= \htmlLks ->
    case htmlLks of
        Nothing  -> return Nothing
        Just lks -> return lks >>= return . Just . (foldl' segregateLkGrp emptyLG) . map (getLinkType url)

    where segregateLkGrp :: LinkGroup -> LinkType -> LinkGroup
          segregateLkGrp grp lt =
              case lt of
                  Outgoing   url -> LinkGroup ([url] ++ (outgoing grp)) (sameDomain grp) (ads grp)
                  SameDomain url -> LinkGroup (outgoing grp) ([url] ++ (sameDomain grp)) (ads grp)
                  Advert     url -> LinkGroup (outgoing grp) (sameDomain grp) ([url] ++ (ads grp))


----------------------------------------------------------------------------------------------------

-- gets the link type given a url and the url of the page containing it

getLinkType :: String -> String -> LinkType
getLinkType url lk = let (a, b) = ((isSameDomain url lk), (isAdvert lk))
                     in case (a, b) of
                            (Just True, _) -> SameDomain lk
                            (_, Just True) -> Advert lk
                            _              -> Outgoing lk


    where isSameDomain :: String -> String -> Maybe Bool
          isSameDomain a b =
              let mRegx = compileM (B.pack "\\.\\S*?\\.") []
              in case mRegx of
                     Left _     -> Nothing
                     Right regx ->
                         match regx (B.pack a) [exec_anchored] >>= return . flip (!!) 0 >>= \_a ->
                         match regx (B.pack b) [exec_anchored] >>= return . flip (!!) 0 >>= \_b ->
                         return $ _a == _b

          isAdvert :: String -> Maybe Bool
          isAdvert a = let mRegx = compileM (B.pack "\\.ad") []
                       in case mRegx of
                              Left _     -> Nothing
                              Right regx -> match regx (B.pack a) [] >>= return . null


----------------------------------------------------------------------------------------------------

-- send Get request for HTML, and extract all links (<a href="xxx"> </a>)

htmlLinks :: String -> IO (Maybe [String])
htmlLinks url =
    get url >>= \r ->
    case r of
        Nothing   -> return Nothing
        Just body -> parseLinks body url >>= return . Just


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

parseLinks :: String -> String -> IO [String]
parseLinks body url = runX (readHtml body >>> (linkify url)) >>= return . (flip (!!) 0)


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

linkify :: ArrowXml a => String -> a XmlTree [String]
linkify url = proc tree -> do
    lks <- listA (atTagCase "a" >>> getAttrValue "href") -< tree
    returnA <<< arr (mapMaybe (expand url)) -< lks

    where expand :: String -> String -> Maybe String
          expand url lk = case isAbsoluteURI lk of
                              True  -> Just lk
                              False -> let root = parseURI $ "http:" ++ lk
                                           uri  = fromJust $ parseURI url
                                      in root >>= flip nonStrictRelativeTo uri >>= \x ->
                                         Just $ uriToString id x ""


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
