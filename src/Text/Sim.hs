module Text.Sim (

) where


import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Control.Parallel
import Viterbi


type ByteString = B.ByteString
type Map = M.Map
----------------------------------------------------------------------------------------------------

-- NOTE: Only works on text suitable for viterbi. For word collections, use #doubleCartweekStd
--
-- determines the similarities of two blocks of text.
-- Extract the nouns from both texts.
-- Assign a weight to each noun for each text, derived from (noun_count / total_nouns)
-- thinking of the noun => weight pairs as a Dictionary, similarity score is:
--    unionWith (*) a b

doubleCartwheelEng :: Vit -> String -> String -> IO Float
doubleCartwheelEng vit a b =
    let (tallyA, tallyB) = (M.empty, M.empty)
        (nounsA, nounsB) = (score tallyA vit a, score tallyB vit b)
        (scoreA, scoreB) = (toPercent scoreA, toPercent scoreB)
    in (force scoreA) `par` (force ())

        where score :: Map ByteString Int -> Vit -> String -> Map ByteString Int
              score tally vit a = L.map (\x -> M.insertWith (+) x 1 tally) nouns $ tag vit (B.pack a)

              toPercent :: Map ByteString Int -> Map ByteString Float
              toPercent x =
                  let ttl = M.foldl' (+) 0 x
                  in M.map (\x -> (fromIntegral x) / ttl) x



