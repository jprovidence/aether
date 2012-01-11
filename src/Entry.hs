module Entry (
    ordDateAsc
,   ordDateDesc
,   toDate
,   date'
,   fromStrDate
,   Entry(Entry, description, date, title, link)
) where


import Data.DateTime
import Data.Time.Format
import Data.Time.LocalTime
import Data.Maybe
import System.Locale
import Data.Ord
import qualified Data.List as L




-- NOTE: This module is often implemented in close relation to the Feed module.
-- An overview of the functioning of this module, as well as Feed can be found at:
-- https://github.com/jprovidence/aether/blob/master/notes/document_corpus.md

----------------------------------------------------------------------------------------------------

-- Data type to contain information describing a single article/document in an RSS or Atom feed.

data Entry = Entry { description :: String
                   , title       :: String
                   , link        :: String
                   , date        :: String
                   } deriving Show


----------------------------------------------------------------------------------------------------

-- Convert a date stored as a String in PostgreSql style to the DateTime format. This allows for
-- easier comparison and sorting of entries.

toDate :: String -> DateTime
toDate d =
    let rawdate = filter (\x -> x /= ' ' && x /= ':' && x /= '-') d
        year    = toInteger (read $ take 4 rawdate    :: Int)
        month   = (read $ take 2 $ after 4 rawdate)   :: Int
        day     = (read $ take 2 $ after 6 rawdate)   :: Int
        hour    = (read $ take 2 $ after 8 rawdate)   :: Int
        minutes = (read $ take 2 $ after 10 rawdate)  :: Int
        seconds = (read $ take 2 $ after 12 rawdate)  :: Int
    in fromGregorian year month day hour minutes seconds

    where after :: Int -> [a] -> [a]
          after i xs = drop i xs


----------------------------------------------------------------------------------------------------

-- evaluates to the publication date of the provided article

date' :: Entry -> DateTime
date' = toDate . date


----------------------------------------------------------------------------------------------------

-- Converts a from String date (in the format usually found in RSS/Atom feeds to the DateTime format

fromStrDate :: String -> DateTime
fromStrDate str =
    let stim = fromJust ((parseTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S +0000" str) :: Maybe ZonedTime)
    in toDate $ show stim


----------------------------------------------------------------------------------------------------

-- Order a list of Entries by their publication date, oldest -> newest

ordDateAsc :: [Entry] -> [Entry]
ordDateAsc es = L.sortBy (comparing $ toDate . date) es


----------------------------------------------------------------------------------------------------

-- Order a list of Entries by their publication date, newest -> oldest

ordDateDesc :: [Entry] -> [Entry]
ordDateDesc = L.reverse . ordDateAsc


----------------------------------------------------------------------------------------------------
