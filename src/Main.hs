module Main (
    main
) where


import qualified Data.ByteString.Char8 as B
import Data.Maybe
import qualified Control.Exception as EX
import Database.HDBC
import Database.HDBC.PostgreSQL
import Feed
import Parse


main = do
    ents <- listEntries "http://feeds.feedburner.com/OpenCulture?fmt=xml"
    case ents of
        Nothing -> putStrLn "Nothing"
        Just x  -> putStrLn $ show x







