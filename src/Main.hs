module Main (
    main
) where


import qualified Data.ByteString.Char8 as B
import Data.Maybe
import qualified Control.Exception as EX
import System.IO.Unsafe
import Feed
import Parse
import Entry
import Communication
import Viterbi
import Text.Sim


textA = unsafePerformIO $ readFile "./test_texts/textA.txt"
textB = unsafePerformIO $ readFile "./test_texts/textB.txt"


main = do
    testTextSim
    --tag vit (B.pack stringOfText) >>= nouns >>= putStrLn . show
    --t <- htmlText "http://www.zerohedge.com/news/guest-post-circling-black-swans-2012"
    --putStrLn $ show t
    --lks <- htmlLinks "http://www.zerohedge.com/news/guest-post-circling-black-swans-2012"
    --return lks >>= putStrLn . show . fromJust
    --communicate


testTextSim :: IO ()
testTextSim = do
    putStrLn "Training Viterbi"
    vit <- trainVit

    putStrLn "Nouns A"
    tag vit (B.pack textA) >>= nouns >>= putStrLn . show

    putStrLn "Nouns B"
    tag vit (B.pack textB) >>= nouns >>= putStrLn . show

    putStrLn "totalRelative byIntersection euclideanDistance"
    sim <- vbaSimilarityVit vit textA textB totalRelative byIntersection euclideanDistance
    putStrLn $ show sim

    putStrLn "invTotalRelative byIntersection euclideanDistance"
    sim <- vbaSimilarityVit vit textA textB invTotalRelative byIntersection euclideanDistance
    putStrLn $ show sim

    putStrLn "countRelative byIntersection euclideanDistance"
    sim <- vbaSimilarityVit vit textA textB countRelative byIntersection euclideanDistance
    putStrLn $ show sim

    putStrLn "invCountRelative byIntersection euclideanDistance"
    sim <- vbaSimilarityVit vit textA textB invCountRelative byIntersection euclideanDistance
    putStrLn $ show sim





