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
import Entry
import Communication


main = do
    --updateFeed "http://feeds.feedburner.com/OpenCulture?fmt=xml"
    h <- htmlLinks "http://feedproxy.google.com/~r/OpenCulture/~3/Rbryt9iinus/john_lennon_sums_up_elvis.html"
    case h of
        Nothing -> putStrLn "Nothing"
        Just x  -> putStrLn $ show x
    putStrLn "Done"







