
module CtDataAccess(connectAndDo, getNextSequenceValue, toSqlTimeStamp, toSqlMaybe, readDbConfig) where

import Database.HDBC 
import Database.HDBC.PostgreSQL 
import System.Time (CalendarTime, formatCalendarTime)
import System.Locale (defaultTimeLocale)
import System.Log.Logger
import System.IO.Error
import System.IO.Unsafe(unsafePerformIO)
import System.Directory

logbase = "CtDataAccess"

data DbConfig = DbConfig { dbName :: String, password :: String} 
                deriving Show

-- | lit le fichier de configuration pour r�cup�rer le nom de la base de donn�e et le password
readDbConfig = DbConfig{dbName = readLines!!0, password = readLines!!1}
    where readLines = lines $ unsafePerformIO $ do homedir <- getHomeDirectory 
                                                   readFile $ homedir ++ "/.ctfile"

-- | connecte � la base puis ex�cute une action
connectAndDo :: DbConfig ->(Connection -> IO a) ->IO()
connectAndDo dbconfig action  = do
  debugM logbase "Connection to database"
  c <- connectPostgreSQL $ "dbname="++ dbName dbconfig ++" port=5432 password="++ password dbconfig ++" connect_timeout=3"
  action c
  debugM logbase "Disconnection from database"
  disconnect c

-- | retourne la prochaine valeur de la s�quence
getNextSequenceValue :: Connection -> String -> IO(SqlValue)
getNextSequenceValue c sequence_name = handleSqlError $ do
  st <- prepare c $ "SELECT nextval('" ++ sequence_name ++ "')" 
  execute st []
  row <- fetchRow st 
  finish st
  case row of Just (folderid:_) -> return folderid
              _                 -> ioError $ userError ("error on getting nextval of "  ++ sequence_name)

-- | convertit une date en SqlValue pour la base de donn�e.
toSqlTimeStamp :: CalendarTime -> SqlValue
toSqlTimeStamp calendar = toSql $ formatCalendarTime defaultTimeLocale "%Y-%b-%d %H:%M:%S" calendar

-- | d�sencapsule un maybe SqlValue
-- renvoit un SqlNull si le param�tre est Nothing
toSqlMaybe
  :: Maybe SqlValue   -- ^ un Maybe encapsulant une SqlValue
  -> SqlValue         -- ^ renvoit la valeur encapsul�e ou SqlNull
toSqlMaybe (Just val) = val
toSqlMaybe Nothing    = SqlNull
