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
import Control.Concurrent


textA = unsafePerformIO $ readFile "./test_texts/textA.txt"
textB = unsafePerformIO $ readFile "./test_texts/textB.txt"


main = do
    putStrLn "\n -- Welcome to the Ether -- \n\n -- Experimental Tier -- \n"
    putStrLn ">> Please select what action you would like to perform. \n>> Help is available with -h"
    processUsrCommands



processUsrCommands :: IO ()
processUsrCommands = do
    input <- getLine
    case input of
        "-h" -> do
            printHelp
            processUsrCommands
        "-fi" -> communicate
        "-c" -> putStrLn ""
        "-t" -> runAllTests
        "-testTextSim" -> testTextSim
        _    -> putStrLn ">> Sorry, your input was not recognised, please try again."


printHelp :: IO ()
printHelp = do
    putStrLn ">> -fi : Run this instance as a function server"
    putStrLn ">> -c  : Run this instance as a corpus manager"
    putStrLn ">> -t  : Run all test functions"
    putStrLn ">> -testTextSim : Run tests of text similarity functions"


runAllTests :: IO ()
runAllTests = do
    testTextSim
    --tag vit (B.pack stringOfText) >>= nouns >>= putStrLn . show
    --t <- htmlText "http://www.zerohedge.com/news/guest-post-circling-black-swans-2012"
    --putStrLn $ show t
    --lks <- htmlLinks "http://www.zerohedge.com/news/guest-post-circling-black-swans-2012"
    --return lks >>= putStrLn . show . fromJust
    --communicate


testTextSim :: IO ()
testTextSim = do
    putStrLn ">> Training Viterbi"
    vit <- trainVit

    putStrLn ">> Nouns A"
    tag vit (B.pack textA) >>= nouns >>= putStrLn . show

    putStrLn ">> Nouns B"
    tag vit (B.pack textB) >>= nouns >>= putStrLn . show

    putStrLn ">> totalRelative byIntersection euclideanDistance"
    sim <- vbaSimilarityVit vit textA textB totalRelative byIntersection euclideanDistance
    putStrLn $ show sim

    putStrLn ">> invTotalRelative byIntersection euclideanDistance"
    sim <- vbaSimilarityVit vit textA textB invTotalRelative byIntersection euclideanDistance
    putStrLn $ show sim

    putStrLn ">> countRelative byIntersection euclideanDistance"
    sim <- vbaSimilarityVit vit textA textB countRelative byIntersection euclideanDistance
    putStrLn $ show sim

    putStrLn ">> invCountRelative byIntersection euclideanDistance"
    sim <- vbaSimilarityVit vit textA textB invCountRelative byIntersection euclideanDistance
    putStrLn $ show sim





