module Text.Sim (
    similarity
,   proportionalRep
,   cartwheel
,   euclideanDistance
,   DistanceMetric(DistanceMetric)
,   RankingFunction(RankingFunction)
,   TallyFunction(TallyFunction)
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


data DistanceMetric = DistanceMetric (Map ByteString (Float, Float) -> Float)

data RankingFunction = RankingFunction (DistanceMetric ->
                                        Map ByteString Float ->
                                        Map ByteString Float ->
                                        Score)

data TallyFunction = TallyFunction ([ByteString] -> Map ByteString Float)


----------------------------------------------------------------------------------------------------

-- NOTE: Only works on text suitable for viterbi. For word collections, use #doubleCartweekStd
--
-- determines the similarities of two blocks of text.
-- Extract the nouns from both texts.
-- Assign a weight to each noun for each text, derived from (noun_count / total_nouns)
-- thinking of the noun => weight pairs as a Dictionary, similarity score is:
--    intersectionWith (*) a b

similarity :: Vit -> TallyFunction -> RankingFunction -> DistanceMetric -> String -> String -> IO Score
similarity vit (TallyFunction tfunc) (RankingFunction rfunc) dmet a b =
    let sA = tag vit (B.pack a) >>= nouns >>= return . tfunc
        sB = tag vit (B.pack b) >>= nouns >>= return . tfunc
    in sA >>= \scoreA -> sB >>= \scoreB ->
       return $ (forceMap scoreA) `par` ((forceMap scoreB) `pseq` ((rfunc dmet) scoreA scoreB))


----------------------------------------------------------------------------------------------------

-- TALLY FUNCTIONS

----------------------------------------------------------------------------------------------------

proportionalRep :: TallyFunction
proportionalRep = TallyFunction proportionalRep'

proportionalRep' :: [ByteString] -> Map ByteString Float
proportionalRep' xs =
    let tally = L.foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty xs
        ttl   = M.fold (+) 0 tally
    in M.map (flip (/) ttl) tally

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


----------------------------------------------------------------------------------------------------

-- UTILS

----------------------------------------------------------------------------------------------------

forceMap :: Map a b -> ()
forceMap m = force $ M.toList m


force :: [a] -> ()
force xs = (go xs) `pseq` ()

    where go :: [a] -> Int
          go (_:ms) = go ms
          go [] = 1
