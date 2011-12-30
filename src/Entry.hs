module Entry (
    ordDateAsc
,   ordDateDesc
,   toDate
,   date'
,   fromStrDate
,   Entry(Entry, description, date, title)
) where


import Data.DateTime
import Data.Time.Format
import Data.Time.LocalTime
import Data.Maybe
import System.Locale
import Data.Ord
import qualified Data.List as L


data Entry = Entry { description :: String
                   , title       :: String
                   , date        :: String
                   } deriving Show

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


date' :: Entry -> DateTime
date' = toDate . date


fromStrDate :: String -> DateTime
fromStrDate str =
    let stim = fromJust ((parseTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S +0000" str) :: Maybe ZonedTime)
    in toDate $ show stim


ordDateAsc :: [Entry] -> [Entry]
ordDateAsc es = L.sortBy (comparing $ toDate . date) es

ordDateDesc :: [Entry] -> [Entry]
ordDateDesc = L.reverse . ordDateAsc
