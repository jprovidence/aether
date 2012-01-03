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
import Viterbi


main = do
    trainVit >>= putStrLn . show







