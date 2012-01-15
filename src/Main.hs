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
import Text.Nouns
import Control.Concurrent


textA = unsafePerformIO $ readFile "./test_texts/textA.txt"
textB = unsafePerformIO $ readFile "./test_texts/textB.txt"


main = do
    putStrLn "\n -- Welcome to the Ether -- \n\n -- Experimental Tier -- \n"
    processUsrCommands



processUsrCommands :: IO ()
processUsrCommands = do
    putStrLn ">> Please select what action you would like to perform. \n>> Help is available with -h"
    input <- getLine
    case input of
        "-h" -> do
            printHelp
            processUsrCommands
        "-fi" -> communicate
        "-c" -> putStrLn ""
        "-t" -> runAllTests
        "-testTextSim" -> testTextSim
        "-testViterbi" -> testViterbi
        "-testNouns" -> testNouns
        _    -> putStrLn ">> Sorry, your input was not recognised, please try again." >>
               processUsrCommands
    putStrLn "\n>> Would you like to do anything else? Y/n"
    input <- getLine
    case input of
        "y" -> processUsrCommands
        "Y" -> processUsrCommands
        _   -> return ()


printHelp :: IO ()
printHelp = do
    putStrLn ">> -fi : Run this instance as a function server"
    putStrLn ">> -c  : Run this instance as a corpus manager"
    putStrLn ">> -t  : Run all test functions"
    putStrLn ">> -testViterbi : Run tests of the viterbi algorithm"
    putStrLn ">> -testTextSim : Run tests of text similarity functions"
    putStrLn ">> -testNouns : Run tests of the noun clustering functions"


runAllTests :: IO ()
runAllTests = do
    testTextSim
    testViterbi
    --tag vit (B.pack stringOfText) >>= nouns >>= putStrLn . show
    --t <- htmlText "http://www.zerohedge.com/news/guest-post-circling-black-swans-2012"
    --putStrLn $ show t
    --lks <- htmlLinks "http://www.zerohedge.com/news/guest-post-circling-black-swans-2012"
    --return lks >>= putStrLn . show . fromJust
    --communicate


testViterbi :: IO ()
testViterbi = do
    putStrLn ">> Training Viterbi \n"
    vit <- trainVit

    putStrLn ">> All tags:\n"
    tag vit (B.pack textA) >>= putStrLn . show
    putStrLn "\n\n----------------------------------------\n\n"

    putStrLn ">> Nouns: \n"
    tag vit (B.pack textA) >>= nouns >>= putStrLn . show
    putStrLn "\n\n----------------------------------------\n\n"

    putStrLn ">> Nouns + Indices: \n"
    tag vit (B.pack textA) >>= nounsAndIndices >>= putStrLn . show


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



testNouns :: IO ()
testNouns = do
    testInitialPositioning

testInitialPositioning :: IO ()
testInitialPositioning = putStrLn $ show $ initialPositioning 111 10.0


