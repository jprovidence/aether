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
import Text.Nouns.SimpleForceCluster
import Text.Nouns.SpringCluster
import Text.Nouns.Graph
import Control.Concurrent
import Utility


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
        "-tag" -> runTagger
        "-addFeed" -> addFeeds
        "-precluster" -> precluster
        "-nvis" -> runNounVisualization
        "-testTextSim" -> testTextSim
        "-testViterbi" -> testViterbi
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
    putStrLn ">> -tag : Tag a document"
    putStrLn ">> -addFeed : Add feeds to the index manually"
    putStrLn ">> -precluster : Perform some pre-cluster processing"
    putStrLn ">> -nvis : Run noun visualizations"
    putStrLn ">> -testViterbi : Run tests of the viterbi algorithm"
    putStrLn ">> -testTextSim : Run tests of text similarity functions"



runTagger :: IO ()
runTagger = do
    putStrLn ">> Training Viterbi, please wait..."
    vit <- trainVit
    putStrLn ">> Training Complete."
    doTag vit

doTag :: Vit -> IO ()
doTag vit = do
    putStrLn ">> Enter the id of the document you would like to tag."
    input <- getLine
    putStrLn (">> Will tag document with id " ++ input ++ ".")
    entry <- entryFromId (read input)
    res <- withFilter (tag vit) $ B.pack $ description entry
    putStrLn ">> Tagged Document: \n\n"
    putStrLn (show res)
    putStrLn "\n\n>> Tag Another? y/n"
    input <- getLine
    case (input == "y") || (input == "Y") of
        True  -> doTag vit
        False -> return ()


addFeeds :: IO ()
addFeeds = do
    putStrLn ">> Please enter the feed URL"
    input <- getLine
    putStrLn (">> Adding feed data at " ++ input ++ ", please wait...")
    assimilateFeed input
    putStrLn ">> Would you like to add another? y/n"
    input <- getLine
    case input of
        "y" -> addFeeds
        "Y" -> addFeeds
        _   -> return ()


precluster :: IO ()
precluster = do
    putStrLn ">> Converting..."
    textToGraph



runNounVisualization :: IO ()
runNounVisualization = do
    putStrLn ">> What do you want to visualize?"
    putStrLn ">> 1 : Browser Simple Force Clustering."
    putStrLn ">> 2 : Tulip Undirected Noun Graph."
    putStrLn ">> 3 : Processing Spring Clustering."

    input <- getLine >>= return . read

    adv <- case input of
              1 -> return ""
              2 -> do
                  putStrLn ">> Use descriptive labels? y/n"
                  getLine
              3 -> return ""

    case input of
        1 -> simpleForceCluster "user"
        2 -> case affirmative adv of
                True  -> graphToTulip'
                False -> graphToTulip
        3 -> springCluster


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
    withFilter (tag vit) (B.pack textA) >>= putStrLn . show
    putStrLn "\n\n----------------------------------------\n\n"

    putStrLn ">> Nouns: \n"
    withFilter (tag vit) (B.pack textA) >>= nouns >>= putStrLn . show
    putStrLn "\n\n----------------------------------------\n\n"

    putStrLn ">> Nouns + Indices: \n"
    withFilter (tag vit) (B.pack textA) >>= nounsAndIndices >>= putStrLn . show


testTextSim :: IO ()
testTextSim = do
    putStrLn ">> Training Viterbi"
    vit <- trainVit

    putStrLn ">> Nouns A"
    withFilter (tag vit) (B.pack textA) >>= nouns >>= putStrLn . show

    putStrLn ">> Nouns B"
    withFilter (tag vit) (B.pack textB) >>= nouns >>= putStrLn . show

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


