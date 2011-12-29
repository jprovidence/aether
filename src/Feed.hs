module Feed (
    feedFromUrl
,   feedFromId
,   find
,   transact
,   wrap
,   numEntries
,   lastUpdate
,   entries
) where


import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Maybe
import qualified Control.Exception as EX
import System.IO.Unsafe
import Database.HDBC
import Database.HDBC.PostgreSQL
import Parse


connStr :: String
connStr = "host=localhost dbname=ticket connect_timeout=7 port=5432 user=postgres password=password"

type ByteString = B.ByteString

data Feed = Feed { _url :: String
                 , _id  :: Int
                 } deriving Show


transact :: (Connection -> IO b) -> IO b
transact func = do
    con <- connectPostgreSQL connStr
    ret <- withTransaction con func
    disconnect con
    return ret


wrap :: (Connection -> IO b) -> IO b
wrap func = do
    con <- connectPostgreSQL connStr
    ret <- func con
    disconnect con
    return ret


assimilateFeed :: String -> IO ()
assimilateFeed url =
    listEntries url >>= \es ->
    case es of
        Nothing -> putStrLn "_ticket: No entries found"
        Just xs -> do
            con <- connectPostgreSQL connStr
            saveMstr con xs url
            mapM_ (save con url) xs
            disconnect con

saveMstr :: Connection -> [Entry] -> String -> IO (Integer)
saveMstr con es url = do
    let a = "'" ++ (show $ length es) ++ "'"
        b = "'" ++ (show $ date (es !! 0)) ++ "'"
        c = "'" ++ url ++ "'"
        d = concat $ L.intersperse ", " [a, b, c]
    sql <- return $  "insert into feeds (num_entries, last_update, url) values (" ++ d ++ ");"
    withTransaction con (\c -> prepare c sql >>= flip execute [])

save :: Connection -> String -> Entry -> IO (Integer)
save con url e = do
    let a = description e
        b = unsafePerformIO (feedFromUrl url >>= return . show . _id . fromJust)
        c = date e
        d = concat $ L.intersperse ", " [a, b, c]
    sql <- return $ "insert into entries (content, feed_id, date) values (" ++ d ++ ");"
    withTransaction con (\c -> prepare c sql >>= flip execute [])



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


find :: Feed -> (Int -> IO a) -> IO a
find fd func = func $ _id fd


numEntries :: Int -> IO Int
numEntries i = wrap (numEntries' i)

    where numEntries' :: Int -> Connection -> IO Int
          numEntries' i con = do
              let sel = "select num_entries from feeds where id=" ++ (show i) ++ ";"
              rows <- quickQuery' con sel []
              mapM (return . fromJust . fromSql . (flip (!!) 0)) rows >>= return . flip (!!) 0


lastUpdate :: Int -> IO String
lastUpdate i = wrap (lastUpdate' i)

    where lastUpdate' :: Int -> Connection -> IO String
          lastUpdate' i con = do
              let sel = "select last_update from feeds where id=" ++ (show i) ++ ";"
              rows <- quickQuery' con sel []
              mapM (return . fromJust . fromSql . (flip (!!) 0)) rows >>= return . flip (!!) 0


entries :: Int -> IO [ByteString]
entries i = wrap (entries' i)

    where entries' :: Int -> Connection -> IO [ByteString]
          entries' i con = do
              let sel = "select * from entries where feed_id=" ++ (show i) ++ ";"
              rows <- quickQuery' con sel []
              mapM (return . B.pack . fromJust . fromSql . (flip (!!) 1)) rows
