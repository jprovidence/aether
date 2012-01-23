module Text.Sim (
    vbaSimilarityVit
,   totalRelative
,   invTotalRelative
,   countRelative
,   invCountRelative
,   byIntersection
,   byInjection
,   euclideanDistance
,   cosineSimilarity
,   chebyshevDistance
,   TallyF(TallyF)
,   DimensionF(DimensionF)
,   DistanceF(DistanceF)
) where


import Data.Ord
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Control.Parallel
import Viterbi


----------------------------------------------------------------------------------------------------

-- typedefs

type ByteString = B.ByteString

type Map = M.Map




----------------------------------------------------------------------------------------------------

-- Vector Based Algorithms (VBAs)

----------------------------------------------------------------------------------------------------

-- Data Structures

----------------------------------------------------------------------------------------------------

-- Data structure representing the similarity of two documents
-- @score@     -> The 'distance' between the two documents.

type Score = Float


----------------------------------------------------------------------------------------------------

-- Boxing for VBA sub-functions that determine term frequency and converts to significance scores

newtype TallyF = TallyF ([ByteString] -> Map ByteString Float)


----------------------------------------------------------------------------------------------------

-- Boxing for VBA sub-functions that equalize the number of dimensions between two document
-- vectors

newtype DimensionF =
    DimensionF (Map ByteString Float -> Map ByteString Float -> Map ByteString (Float, Float))


----------------------------------------------------------------------------------------------------

-- Boxing for VBA subfunctions that calculate some measure of distance between two vectors

newtype DistanceF = DistanceF (Map ByteString (Float, Float) -> Score)




----------------------------------------------------------------------------------------------------

-- Governing Functions

----------------------------------------------------------------------------------------------------

-- NOTE: Only works on text suitable for viterbi. For word collections, use #vbaSimilarityStd
--
-- Higher-order function to govern the composition of VBA sub-functions, and to structure them
-- such that they may execute in parallel.

vbaSimilarityVit :: Vit -> String -> String -> TallyF -> DimensionF -> DistanceF -> IO Score
vbaSimilarityVit vit a b (TallyF tallyf) (DimensionF dimenf) (DistanceF distf) =
    let getSim x y = return (dimenf x y) >>= return . distf
        calcScore x = withFilter (tag vit) (B.pack x) >>= nouns >>= return . tallyf
        scores = sweep (calcScore a, calcScore b)
    in scores >>= \(sa, sb) -> (forceMap sa) `par` ((forceMap sb) `pseq` (getSim sa sb))


----------------------------------------------------------------------------------------------------

-- Utility function, similar to "sequence :: [m a] -> m [a]", but for two-tuples

sweep :: Monad m => (m a, m b) -> m (a, b)
sweep (a, b) = a >>= \_a -> b >>= \_b -> return (_a, _b)


----------------------------------------------------------------------------------------------------

-- Utility function, force strict evaluation of a Map

forceMap :: Map a b -> ()
forceMap m = force $ M.toList m


----------------------------------------------------------------------------------------------------

-- Utility function, force strict evaluation of a List

force :: [a] -> ()
force xs = (go xs) `pseq` ()

    where go :: [a] -> Int
          go (_:ms) = go ms
          go [] = 1




----------------------------------------------------------------------------------------------------

-- Tally Sub-Functions

----------------------------------------------------------------------------------------------------

-- Boxes the function #totalRelative' in a TallyF type

totalRelative :: TallyF
totalRelative = TallyF totalRelative'


----------------------------------------------------------------------------------------------------

-- Determine the frequency of each distinct term in the argument @xs@ :: [ByteString]
-- Map each word to its frequency, convert these frequencies into a significance based on the
-- following generalization:
-- significance = term frequency / (length @xs@)

totalRelative' :: [ByteString] -> Map ByteString Float
totalRelative' xs = let tally = wordCounts xs
                        ttl   = M.fold (+) 0 tally
                    in M.map (flip (/) ttl) tally


----------------------------------------------------------------------------------------------------

-- Performs significance attribution by mapping ((/) 1) over each result of totalRelative'

invTotalRelative :: TallyF
invTotalRelative = TallyF (M.map ((/) 1) . totalRelative')


----------------------------------------------------------------------------------------------------

-- Boxes the function #countRelative' in a TallyF type

countRelative :: TallyF
countRelative = TallyF countRelative'


----------------------------------------------------------------------------------------------------

-- Determine the frequency of each distinct term in the argument @xs@ :: [ByteString]
-- Map each word to its frequency, convert these frequencies into a significance based on the
-- following generalization:
-- significance = term frequency / (highest term frequency recorded in @xs@)

countRelative' :: [ByteString] -> Map ByteString Float
countRelative' xs = let tally = wordCounts xs
                        maxim = L.maximum $ L.map snd $ M.toList tally
                    in M.map (flip (/) maxim) tally


----------------------------------------------------------------------------------------------------

-- Performs significance attribution by mapping ((/) 1) over each result of countRelative'

invCountRelative :: TallyF
invCountRelative = TallyF (M.map ((/) 1) . countRelative')


----------------------------------------------------------------------------------------------------

-- Utility function to determine frequency of each term

wordCounts :: [ByteString] -> Map ByteString Float
wordCounts bstr = L.foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty bstr




----------------------------------------------------------------------------------------------------

-- Dimension Equalizing Sub-Functions

----------------------------------------------------------------------------------------------------

--

byIntersection :: DimensionF
byIntersection = DimensionF byIntersection'


----------------------------------------------------------------------------------------------------

--

byIntersection' :: Map ByteString Float -> Map ByteString Float -> Map ByteString (Float, Float)
byIntersection' ma mb = M.intersectionWith (\a b -> (a, b)) ma mb


----------------------------------------------------------------------------------------------------

--

byInjection :: DimensionF
byInjection = DimensionF byInjection'


----------------------------------------------------------------------------------------------------

--

byInjection' :: Map ByteString Float -> Map ByteString Float -> Map ByteString (Float, Float)
byInjection' ma mb =
    let blanka = M.map (\x -> (x, 0)) $ M.difference ma mb
        blankb = M.map (\x -> (0, x)) $ M.difference mb ma
        inters = M.intersectionWith (\a b -> (a, b)) ma mb
    in L.foldl' M.union M.empty [blanka, blankb, inters]




----------------------------------------------------------------------------------------------------

-- Distance Sub-Functions

----------------------------------------------------------------------------------------------------

--

euclideanDistance :: DistanceF
euclideanDistance = DistanceF euclideanDistance'


----------------------------------------------------------------------------------------------------

--

euclideanDistance' :: Map ByteString (Float, Float) -> Score
euclideanDistance' = sqrt . (L.foldl' (+) 0) . (L.map (sqDiff . snd)) . M.toList

    where sqDiff :: (Float, Float) -> Float
          sqDiff (a, b) = (a - b) ^ 2


----------------------------------------------------------------------------------------------------

--

cosineSimilarity :: DistanceF
cosineSimilarity = DistanceF cosineSimilarity'


----------------------------------------------------------------------------------------------------

--

cosineSimilarity' :: Map ByteString (Float, Float) -> Score
cosineSimilarity' m =
    let tuples       = L.map snd $ M.toList m
        dotProduct   = L.foldl' dotStep 0 tuples
        denominator' = (mag $ L.map fst tuples) * (mag $ L.map snd tuples)
    in dotProduct / denominator'

    where dotStep :: Float -> (Float, Float) -> Float
          dotStep acc (a, b) = (a * b) + acc

          mag :: [Float] -> Float
          mag = sqrt . (L.foldl' (\acc x -> (x^2) + acc) 0)


----------------------------------------------------------------------------------------------------

--

chebyshevDistance :: DistanceF
chebyshevDistance = DistanceF chebyshevDistance'


----------------------------------------------------------------------------------------------------

--

chebyshevDistance' :: Map ByteString (Float, Float) -> Score
chebyshevDistance' = L.maximum . (L.map (abs . tplDiff . snd)) . M.toList

    where tplDiff :: (Float, Float) -> Float
          tplDiff (a, b) = a - b


----------------------------------------------------------------------------------------------------
