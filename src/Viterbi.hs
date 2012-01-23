module Viterbi (
    trainVit
,   withFilter
,   nouns
,   nounsAndIndices
,   nounsAndIndices'
,   tag
,   Vit
) where


import qualified Data.HashTable.IO as M
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Maybe
import Data.Text.Encoding
import Control.Monad
import System.IO.Unsafe
import System.Directory




----------------------------------------------------------------------------------------------------

-- DATA STUCTURES

----------------------------------------------------------------------------------------------------

-- type aliases

type Table k v = M.CuckooHashTable k v
type ByteString = B.ByteString
type NestedMap = Table ByteString (Table ByteString Float)


----------------------------------------------------------------------------------------------------

-- data structure to hold the statistics gained from training. Also simplies both the training and
-- tagging process

data Vit = Vit { projections :: NestedMap
               , inherants   :: NestedMap
               , lastTag     :: Maybe ByteString
               } deriving Show




----------------------------------------------------------------------------------------------------

-- TRAINING

----------------------------------------------------------------------------------------------------

-- train a tagger on the brown corpus

trainVit :: IO Vit
trainVit = newVit >>= \v -> corpusFiles >>= mapM B.readFile >>= foldM incTraining v


----------------------------------------------------------------------------------------------------

-- get the path to each file in the corpus

corpusFiles :: IO [String]
corpusFiles = getDirectoryContents "./brown" >>= return . L.map ("./brown/" ++) . L.filter isFile

    where isFile :: String -> Bool
          isFile s = s /= "." && s /= ".."


----------------------------------------------------------------------------------------------------

-- increment the algorithm's training on a single pre-tagged document

incTraining :: Vit -> ByteString -> IO Vit
incTraining vit str =
    let spl = L.filter noEmpty $ B.splitWith onWhiteSp str
    in foldM mergeV vit spl >>= finalize


----------------------------------------------------------------------------------------------------

-- finalize the training session so the algorithm may be used for tagging.

finalize :: Vit -> IO Vit
finalize v =
    let proj = projections v
        inhe = inherants v
    in M.mapM_ toPercent proj >> M.mapM_ toPercent inhe >> return v

    where toPercent :: (ByteString, Table ByteString Float) -> IO ()
          toPercent (_, m) = let ttl = M.foldM (\acc (_, v) -> return $ acc + v) 0 m
                              in ttl >>= \t -> M.mapM_ (\(k, v) -> return $ (v / t) * 100) m


----------------------------------------------------------------------------------------------------

-- add a single word/tag pair to the viterbi's statistics

mergeV :: Vit -> ByteString -> IO Vit
mergeV v str =
    let spl = B.split '/' str
    in add (lastTag v) (spl !! 1) (projections v) >>= \a ->
       add (Just $ spl !! 0) (spl !! 1) (inherants v) >>= \b ->
       return $ Vit a b (Just $ spl !! 1)


----------------------------------------------------------------------------------------------------

-- resolves how to insert a given word/tag pair into a NestedMap

add :: Maybe ByteString -> ByteString -> NestedMap -> IO NestedMap

add Nothing tag m = return m

add (Just wd) tag m
    | depthOfGiven m wd tag == 0 = M.new >>= \a -> M.insert a tag 1 >> M.insert m wd a >> return m
    | depthOfGiven m wd tag == 1 = m `M.lookup` wd >>= deJust >>= \a -> M.insert a tag 1 >> return m
    | depthOfGiven m wd tag == 2 = m `M.lookup` wd >>= deJust >>= \a -> a `M.lookup` tag >>= deJust >>= \i ->
                                  M.insert a tag (1 + i) >> return m

    where deJust :: Maybe a -> IO a
          deJust = return . fromJust


----------------------------------------------------------------------------------------------------

-- determines whether a specific word and tag has been encoutered already

depthOfGiven :: NestedMap -> ByteString -> ByteString -> Int
depthOfGiven m word tag = unsafePerformIO $
    m `M.lookup` word >>= \res ->
    case res of
        Nothing -> return 0
        Just x  -> x `M.lookup` tag >>= \res1 ->
                  case res1 of
                      Nothing -> return 1
                      Just y  -> return 2




----------------------------------------------------------------------------------------------------

-- APPLICATION

----------------------------------------------------------------------------------------------------

-- extract only nouns

nouns :: ByteString -> IO [ByteString]
nouns = return . (mapMaybe isNoun) . (L.filter noEmpty) . B.split ' '


----------------------------------------------------------------------------------------------------

-- extract nouns and their index in a given document

nounsAndIndices :: ByteString -> IO [(ByteString, Int)]
nounsAndIndices = (liftM L.reverse) . nounsAndIndices'


----------------------------------------------------------------------------------------------------

-- extract nouns and their index in a given document. Will be faster than #nounsAndIndicies, as it
-- does not bother to reverse the list. Nouns/indices will be backward order

nounsAndIndices' :: ByteString -> IO [(ByteString, Int)]
nounsAndIndices' = return . fst . (L.foldl' accFunc ([], 0)) . (L.filter noEmpty) . B.split ' '

    where accFunc :: ([(ByteString, Int)], Int) -> ByteString -> ([(ByteString, Int)], Int)
          accFunc (lst, idx) x = case isNoun x of
                                     Nothing -> (lst, idx + 1)
                                     Just n  -> ([(n, idx)] ++ lst, idx + 1)


----------------------------------------------------------------------------------------------------

-- tag a given string with its parts of speech

tag :: Vit -> [ByteString] -> IO ByteString
tag unv lst = do
    v <- return (clear unv)
    liftM snd $ foldM resolve (v, B.empty) lst


----------------------------------------------------------------------------------------------------

-- resolve the part of speech of a given word, based on its context and training statistics

resolve :: (Vit, ByteString) -> ByteString -> IO (Vit, ByteString)
resolve (v, last) str
    | isBlank (lastTag v) = (inherants v) `M.lookup` str >>= greatestSingleTag >>= \x ->
                            return $ (updateVitLast v x, completeAppend last str x)
    | otherwise =
        case lastTag v of
            Nothing -> do
                x <- (inherants v) `M.lookup` str >>= greatestSingleTag
                return $ (updateVitLast v x, completeAppend last str x)
            Just t  -> do
                pr <- (projections v) `M.lookup` t
                ih <- (inherants v) `M.lookup` str
                tg <- greatestMutualTag pr ih
                return $ (updateVitLast v tg, completeAppend last str tg)

    where completeAppend :: ByteString -> ByteString -> ByteString -> ByteString
          completeAppend para str tg =
              (para `B.snoc` ' ') `B.append` ((str `B.snoc` '/') `B.append` tg)


----------------------------------------------------------------------------------------------------

-- extract the most probable tag in a single tag

greatestSingleTag :: Maybe (Table ByteString Float) -> IO ByteString

greatestSingleTag Nothing = return $ B.pack "unk"

greatestSingleTag (Just m) = liftM fst $ M.foldM compareProb (B.pack "unk", 0) m

    where compareProb :: (ByteString, Float) -> (ByteString, Float) -> IO (ByteString, Float)
          compareProb (t, acc) (k, v) = case acc < v of
                                            True  -> return (k, v)
                                            False -> return (t, acc)


----------------------------------------------------------------------------------------------------

-- extract the most probable tag from the combination of two tables

greatestMutualTag :: Maybe (Table ByteString Float) -> Maybe (Table ByteString Float) -> IO ByteString

greatestMutualTag Nothing Nothing = return (B.pack "unk")

greatestMutualTag Nothing (Just m) = greatestSingleTag (Just m)

greatestMutualTag (Just m) Nothing = greatestSingleTag (Just m)

greatestMutualTag (Just pr) (Just ih) =
    M.foldM (lrgMutual pr) (B.pack "unk", 0) ih >>= return . fst >>= \x ->
    case x == B.pack "unk" of
        True -> greatestSingleTag (Just ih)
        _    -> return x


----------------------------------------------------------------------------------------------------

-- return the most likely tag given two viterbi tables. For use with fold, see #greatestMutualTag

lrgMutual :: Table ByteString Float -> (ByteString, Float) -> (ByteString, Float) -> IO (ByteString, Float)
lrgMutual tbl acc (tag, pc) = tbl `M.lookup` tag >>= \res ->
                               case res of
                                   Nothing    -> return acc
                                   Just match -> case (snd acc) < (pc * match) of
                                       True  -> return $ (tag, pc * match)
                                       False -> return acc


----------------------------------------------------------------------------------------------------

-- Filter

----------------------------------------------------------------------------------------------------

withFilter :: ([ByteString] -> IO ByteString) -> ByteString -> IO ByteString
withFilter func doc =
    let preprocessed = mapMaybe validateUTF $ B.splitWith onWhiteSp doc
        clean = L.foldl' handlePunc [] $ L.map stringCleaners $ L.filter listLvlFilters preprocessed
    in func clean >>= return . noComments




onWhiteSp :: Char -> Bool
onWhiteSp w = w == ' ' || w == '\n' || w == '\t'

validateUTF :: ByteString -> Maybe ByteString
validateUTF b = case decodeUtf8' b of
                    Left  _ -> Nothing
                    Right _ -> Just b

listLvlFilters :: ByteString -> Bool
listLvlFilters b = (noEmpty b) && (noHTML b) && (noAttr b)

noEmpty :: ByteString -> Bool
noEmpty b = (b /= B.pack " ") && (b /= B.empty)

noHTML :: ByteString -> Bool
noHTML b = ('<' `B.notElem` b) && ('>' `B.notElem` b)

noAttr :: ByteString -> Bool
noAttr b = '=' `B.notElem` b

stringCleaners :: ByteString -> ByteString
stringCleaners = rmRelics . noNumb . noPunc

noNumb :: ByteString -> ByteString
noNumb = B.filter (\x -> not (x `L.elem` numbers))

noPunc :: ByteString -> ByteString
noPunc str = B.filter (\b -> not (b `L.elem` punctuationMarks)) str

rmRelics :: ByteString -> ByteString
rmRelics str
    | (B.pack "div") `B.isSuffixOf` str = B.take (blen - 3) str
    | (B.pack "br") `B.isSuffixOf` str = B.take (blen - 2) str
    | otherwise = str

    where blen = B.length str


-- many data sources are blogs. The word 'comments' appears often in a context unrelated to the
-- article.

noComments :: ByteString -> ByteString
noComments =
    L.foldl' repair (B.pack "") . L.filter containsComment . L.filter noEmpty . B.splitWith onWhiteSp

    where containsComment str = not $ ((B.splitWith (== '/') str) !! 0) == (B.pack "comments")
          repair acc x = acc `B.append` (x `B.snoc` ' ')


-- unsure whether this helps

handlePunc :: [ByteString] -> ByteString -> [ByteString]
handlePunc acc x =
    acc ++ (L.foldl' (\_acc _x -> _acc ++ [x]) [] $ B.splitWith (flip L.elem punctuationMarks) x)



----------------------------------------------------------------------------------------------------

-- UTILS

----------------------------------------------------------------------------------------------------

-- determine whether a tag is capable of contributing a projection

isBlank :: Maybe ByteString -> Bool
isBlank Nothing = False
isBlank (Just str) = str == B.empty || str == (B.pack "unk")


----------------------------------------------------------------------------------------------------

-- evaluates to (Just word) if str is a noun, (Nothing) otherwise

isNoun :: ByteString -> Maybe ByteString
isNoun str = let lst = B.split '/' str
                 wd  = lst !! 0
                 tg  = lst !! 1
             in case tg `L.elem` (L.map B.pack nounTags) of
                    True  -> Just wd
                    False -> Nothing

----------------------------------------------------------------------------------------------------

-- reset the lastWord field of a Vit

clear :: Vit -> Vit
clear v = Vit (projections v) (inherants v) Nothing


----------------------------------------------------------------------------------------------------

-- update the lastWord field of a Vit

updateVitLast :: Vit -> ByteString -> Vit
updateVitLast v str = Vit (projections v) (inherants v) (Just str)


----------------------------------------------------------------------------------------------------

-- constructs a new Viterbi data structure

newVit :: IO Vit
newVit = M.new >>= \a -> M.new >>= \b -> return $ Vit a b Nothing


----------------------------------------------------------------------------------------------------

-- punctuation

punctuationMarks :: [Char]
punctuationMarks = ['.', ',', '"', '\'', ':', ';', '(', ')', ']', '[', '\\', '/', '-', '+', '&',
                    '?', '!', '$', '@', '<', '>', '{', '}', '%', '#']


----------------------------------------------------------------------------------------------------

-- all noun tags in the Viterbi corpus tag-setAllowBasicAuth

nounTags :: [String]
nounTags = [ "fw-at+nn", "fw-at+np", "fw-in+nn", "fw-in+np", "fw-nn", "fw-nn$", "fw-nns", "fw-np",
             "fw-nps", "fw-nr", "nn", "nn$", "nn+bez", "nn+hvd", "nn+hvz", "nn+in", "nn+md", "nn+nn",
             "nns", "nns$", "nns+md", "np", "np$", "np+bez", "np+hvz", "np+md", "nps", "nps$", "nr",
             "nr$", "nr+md", "nrs" ]


numbers :: [Char]
numbers = ['1', '2', '3', '4', '5', '6', '7', '8', '9']


upperCases :: [Char]
upperCases = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q',
              'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']

