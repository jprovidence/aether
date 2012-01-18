module Feed (
    assimilateFeed
,   feedFromUrl
,   feedFromId
,   find
,   transact
,   wrap
,   allFeeds
,   updateFeed
,   overflow
,   numEntries
,   lastUpdate
,   entries
,   entryFromId
,   Feed(Feed, _id, _url)
) where


import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Maybe
import Control.Monad
import Control.Concurrent
import qualified Control.Exception as EX
import System.IO.Unsafe
import Database.HDBC
import Database.HDBC.PostgreSQL
import Parse
import Entry
import Viterbi
import Utility




-- NOTE: This module is often implemented in close relation to the Entry module.
-- An overview of the functioning of this module, as well as Entry can be found at:
-- https://github.com/jprovidence/aether/blob/master/notes/document_corpus.md

----------------------------------------------------------------------------------------------------

-- typedefs

type ByteString = B.ByteString


----------------------------------------------------------------------------------------------------

-- Data type to represent an RSS/Atom feed

data Feed = Feed { _url :: String
                 , _id  :: Int
                 } deriving Show


----------------------------------------------------------------------------------------------------

-- incorporate a feed url into the overall index

assimilateFeed :: String -> IO ()
assimilateFeed url =
    urlEntries url >>= \es ->
    case es of
        Nothing -> putStrLn ">> No entries found."
        Just xs -> do
            con <- connectPostgreSQL connStr
            saveMstr con xs url
            mapM_ (save con url) xs
            disconnect con
            putStrLn ">> Feed assimilated."


----------------------------------------------------------------------------------------------------

-- Update all feeds every 6 hours

cyclicalUpdate :: IO ()
cyclicalUpdate = do
    fds <- allFeeds
    case fds of
        Nothing -> putStrLn ">> No feeds detected. Ensure database is initialized and functional"
        Just xs -> mapM_ (updateFeed . _url) xs >> threadDelay 21600000000 >> cyclicalUpdate


----------------------------------------------------------------------------------------------------

-- update a feed which has already been indexed

updateFeed :: String -> IO ()
updateFeed url =
    urlEntries url >>= \mes ->
    case mes of
        Nothing -> return ()
        Just es -> do
            overflow url es >>= \ovf -> transact (\c -> mapM_ (save c url) ovf)
            transact (\c -> updateUpdate c url (date $ es !! 0))
            return ()


----------------------------------------------------------------------------------------------------

-- select only those entries at the given url which more recent than the newest entry in the given
-- list

overflow :: String -> [Entry] -> IO [Entry]
overflow url es =
    feedFromUrl url >>= flip find (lastUpdate) . fromJust >>= return . toDate >>= \d ->
    return (L.filter (\x -> d < (fromStrDate $ date x)) es)


----------------------------------------------------------------------------------------------------

-- updates the "last_update" feed in the feeds table row which corresponds to the given url

updateUpdate :: Connection -> String -> String -> IO (Integer)
updateUpdate con url d = do
    let id  = unsafePerformIO $ feedFromUrl url >>= return . show . _id . fromJust
        sql = "update feeds set last_update='" ++ d ++ "' where id=" ++ id ++ ";"
    prepare con sql >>= flip execute []


----------------------------------------------------------------------------------------------------

-- describes the database-interface process required to insert a new feed into postgres

saveMstr :: Connection -> [Entry] -> String -> IO (Integer)
saveMstr con es url = do
    let a = "'" ++ (show $ length es) ++ "'"
        b = "'" ++ (show $ date (es !! 0)) ++ "'"
        c = "'" ++ url ++ "'"
        d = L.intercalate ", " [a, b, c]
    sql <- return $  "insert into feeds (num_entries, last_update, url) values (" ++ d ++ ");"
    withTransaction con (\c -> prepare c sql >>= flip execute [])


----------------------------------------------------------------------------------------------------

-- describes the database-interface process required to insert a new entry into postgres

save :: Connection -> String -> Entry -> IO (Integer)
save con url e = do
    let a = "'" ++ (L.filter noQuot $ description e) ++ "'"
        b = "'" ++ (unsafePerformIO (feedFromUrl url >>= return . show . _id . fromJust)) ++ "'"
        c = "'" ++ (date e) ++ "'"
        d = "'" ++ (L.filter noQuot $ title e) ++ "'"
        f = "'" ++ (link e) ++ "'"
        z = concat $ L.intersperse ", " [a, b, c, d, f]
    sql <- return $ "insert into entries (content, feed_id, date, title, link) values (" ++ z ++ ");"
    withTransaction con (\c -> prepare c sql >>= flip execute [])


----------------------------------------------------------------------------------------------------

-- select all feeds from the database. Not the most efficient implementation, but simplicity is more
-- ideal here

allFeeds :: IO (Maybe [Feed])
allFeeds = EX.try (wrap allFeeds') >>= \res ->
           case (res :: Either EX.SomeException [Feed]) of
               Left  _ -> return Nothing
               Right x -> return $ Just x

    where allFeeds' :: Connection -> IO [Feed]
          allFeeds' con = do
              let sel = "select * from feeds;"
              rows <- quickQuery' con sel []
              fids <- mapM (return . fromSql . (flip (!!) 0)) rows >>= return . catMaybes
              mapM feedFromId fids >>= return . catMaybes


----------------------------------------------------------------------------------------------------

-- given a url, returns this feed as represented from the database, or Nothing if it does not
-- exist

feedFromUrl :: String -> IO (Maybe Feed)
feedFromUrl url = EX.try (wrap (fromUrl url)) >>= \res ->
                  case (res :: Either EX.SomeException (Maybe Feed)) of
                      Left _  -> return Nothing
                      Right x -> return x

    where fromUrl :: String -> Connection -> IO (Maybe Feed)
          fromUrl str con = do
              let sel = "select id from feeds where url='" ++ str ++ "';"
              rows <- quickQuery' con sel []
              fdid <- mapM (return . fromSql . (flip (!!) 0)) rows >>= return . flip (!!) 0
              case fdid of
                  Just x -> return $ Just $ Feed {_url=str, _id=x}
                  _      -> return Nothing


----------------------------------------------------------------------------------------------------

-- given an id, returns a feed as represented in the database, or Nothing if it does not exist

feedFromId :: Int -> IO (Maybe Feed)
feedFromId id = EX.try (wrap (fromId id)) >>= \res ->
                case (res :: Either EX.SomeException (Maybe Feed)) of
                    Left _  -> return Nothing
                    Right x -> return x

    where fromId :: Int -> Connection -> IO (Maybe Feed)
          fromId id con = do
              let sel = "select url from feeds where id=" ++ (show id) ++ ";"
              rows <- quickQuery' con sel []
              furl <- mapM (return . fromSql . (flip (!!) 0)) rows >>= return . flip (!!) 0
              case furl of
                  Just x -> return $ Just $ Feed {_url=x, _id=id}
                  _      -> return Nothing


----------------------------------------------------------------------------------------------------

-- Higher-order function, takes one of #numEntries, #lastUpdate or #entries and a feed as arguments.
-- Abstracts the extraction of row_id from the Feed representation

find :: Feed -> (Int -> IO a) -> IO a
find fd func = func $ _id fd


----------------------------------------------------------------------------------------------------

-- given a feed id, return its `num_entries` field

numEntries :: Int -> IO Int
numEntries i = wrap (numEntries' i)

    where numEntries' :: Int -> Connection -> IO Int
          numEntries' i con = do
              let sel = "select num_entries from feeds where id=" ++ (show i) ++ ";"
              rows <- quickQuery' con sel []
              mapM (return . fromJust . fromSql . (flip (!!) 0)) rows >>= return . flip (!!) 0


----------------------------------------------------------------------------------------------------

-- given a feed id, return its `last_update` field

lastUpdate :: Int -> IO String
lastUpdate i = wrap (lastUpdate' i)

    where lastUpdate' :: Int -> Connection -> IO String
          lastUpdate' i con = do
              let sel = "select last_update from feeds where id=" ++ (show i) ++ ";"
              rows <- quickQuery' con sel []
              mapM (return . fromJust . fromSql . (flip (!!) 0)) rows >>= return . flip (!!) 0


----------------------------------------------------------------------------------------------------

-- given a feed id, return all associated entries field

entries :: Int -> IO [Entry]
entries i = wrap (entries' i)

    where entries' :: Int -> Connection -> IO [Entry]
          entries' i con = do
              let sel  = "select * from entries where feed_id=" ++ (show i) ++ ";"
                  f x  = fromJust . fromSql . (flip (!!) x)
              rows <- quickQuery' con sel []
              return $ map (\e -> Entry {description=(f 1 e), date=(f 3 e), title=(f 4 e), link=(f 5 e)}) rows


----------------------------------------------------------------------------------------------------

-- find an entry by its id

entryFromId :: Int -> IO Entry
entryFromId id = wrap (efID id) >>= return . flip (!!) 0

    where efID :: Int -> Connection -> IO [Entry]
          efID id con = do
              let sel = "select * from entries where id=" ++ (show id) ++ ";"
                  f x  = fromJust . fromSql . (flip (!!) x)
              rows <- quickQuery' con sel []
              return $ map (\e -> Entry {description=(f 1 e), date=(f 3 e), title=(f 4 e), link=(f 5 e)}) rows


----------------------------------------------------------------------------------------------------

-- remove single quotes

noQuot :: Char -> Bool
noQuot c = not (c == '\'' || c == '`')


