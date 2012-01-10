
--
-- WORK ONGOING
--

module Text.Sim (
    similarity
,   totalRelative
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


type ByteString = B.ByteString

type Map = M.Map

data Score = Score { score     :: Float
                   , wordRanks :: [ByteString]
                   } deriving Show



data TallyF = TallyF ([ByteString] -> Map ByteString Float)


data DimensionF = DimensionF (Map ByteString Float ->
                              Map ByteString Float ->
                              (Map ByteString (Float, Float), [ByteString]))


data DistanceF = DistanceF (Map ByteString (Float, Float) -> Float)




----------------------------------------------------------------------------------------------------

-- NOTE: Only works on text suitable for viterbi. For word collections, use #doubleCartweekStd
--
-- determines the similarity of two texts given the provided processing functions

similarity :: Vit -> String -> String -> TallyF -> DimensionF -> DistanceF -> IO Score
similarity vit a b (TallyF tallyf) (DimensionF dimenf) (DistanceF distf) =
    let getSim x y = return (dimenf x y) >>= \(_x, _y) -> return $ Score (distf _x) _y
        calcScore x = tag vit (B.pack x) >>= nouns >>= return . tallyf
        scores = sweep (calcScore a, calcScore b)
    in scores >>= \(sa, sb) -> (forceMap sa) `par` ((forceMap sb) `pseq` (getSim sa sb))




----------------------------------------------------------------------------------------------------

-- TALLY FUNCTIONS

----------------------------------------------------------------------------------------------------

totalRelative :: TallyF
totalRelative = TallyF totalRelative'

totalRelative' :: [ByteString] -> Map ByteString Float
totalRelative' xs = let tally = wordCounts xs
                        ttl   = M.fold (+) 0 tally
                    in M.map (flip (/) ttl) tally


invTotalRelative :: TallyF
invTotalRelative = TallyF (M.map ((-) 1) . totalRelative')



countRelative :: TallyF
countRelative = TallyF countRelative'

countRelative' :: [ByteString] -> Map ByteString Float
countRelative' xs = let tally = wordCounts xs
                        maxim = L.maximum $ L.map snd $ M.toList tally
                    in M.map (flip (/) maxim) tally


invCountRelative :: TallyF
invCountRelative = TallyF (M.map ((-) 1) . countRelative')


wordCounts :: [ByteString] -> Map ByteString Float
wordCounts bstr = L.foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty bstr

{-
----------------------------------------------------------------------------------------------------

-- RANKING FUNCTIONS

----------------------------------------------------------------------------------------------------

cartwheel :: RankingFunction
cartwheel = RankingFunction cartwheel'

cartwheel' :: DistanceMetric -> Map ByteString Float -> Map ByteString Float -> Score
cartwheel' (DistanceMetric met) a b =
     let rankInter = M.toList $ M.intersectionWith (*) a b
         simInter  = M.intersectionWith (\x y -> (x, y)) a b
         ranked    = L.map fst $ L.sortBy (comparing snd) rankInter
     in (force ranked) `par` ((forceMap simInter) `pseq` (Score (met simInter) (ranked)))


----------------------------------------------------------------------------------------------------

-- DISTANCE METRICS

----------------------------------------------------------------------------------------------------

euclideanDistance :: DistanceMetric
euclideanDistance = DistanceMetric euclideanDistance'


euclideanDistance' :: Map ByteString (Float, Float) -> Float
euclideanDistance' = sqrt . (L.foldl' (+) 0) . (L.map (sqDiff . snd)) . M.toList

    where sqDiff :: (Float, Float) -> Float
          sqDiff (a, b) = (a - b) ^ 2

-}
----------------------------------------------------------------------------------------------------

-- UTILS

----------------------------------------------------------------------------------------------------

sweep :: Monad m => (m a, m b) -> m (a, b)
sweep (a, b) = a >>= \_a -> b >>= \_b -> return (_a, _b)


forceMap :: Map a b -> ()
forceMap m = force $ M.toList m


force :: [a] -> ()
force xs = (go xs) `pseq` ()

    where go :: [a] -> Int
          go (_:ms) = go ms
          go [] = 1
