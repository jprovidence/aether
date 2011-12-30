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


main = do
    updateFeed "http://feeds.feedburner.com/OpenCulture?fmt=xml"
    putStrLn "Done"







