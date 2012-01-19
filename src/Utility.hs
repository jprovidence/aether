module Utility (
    wrap
,   transact
,   affirmative
,   connStr
,   pythagoras
) where

import Database.HDBC
import Database.HDBC.PostgreSQL

----------------------------------------------------------------------------------------------------

-- details required to connect to the postgreSQL database. (Password changed for GitHub)

connStr :: String
connStr = "host=localhost dbname=ticket connect_timeout=7 port=5432 user=postgres password=password"


----------------------------------------------------------------------------------------------------

-- wraps database interactions with code require to open/close connection and executes it within a
-- postgres transaction

transact :: (Connection -> IO b) -> IO b
transact func = do
    con <- connectPostgreSQL connStr
    ret <- withTransaction con func
    disconnect con
    return ret


----------------------------------------------------------------------------------------------------

-- wraps database interactions with code require to open/close connection. Code requiring a
-- transaction is not suitable here

wrap :: (Connection -> IO b) -> IO b
wrap func = do
    con <- connectPostgreSQL connStr
    ret <- func con
    disconnect con
    return ret


----------------------------------------------------------------------------------------------------

-- user answered yes?

affirmative :: String -> Bool
affirmative str = (str == "y") || (str == "Y")


----------------------------------------------------------------------------------------------------

-- hypotenuse right angled triangle

pythagoras :: Float -> Float -> Float
pythagoras a b = sqrt ((a^2) + (b^2))
