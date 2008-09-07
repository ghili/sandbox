
module CtDataAccess where

import Database.HDBC 
import Database.HDBC.PostgreSQL 
import System.Time (CalendarTime, formatCalendarTime)
import System.Locale (defaultTimeLocale)
import System.Log.Logger
import System.IO.Error

logbase = "CtDataAccess"

-- | connecte à la base puis exécute une action
connectAndDo
  :: (Connection -> IO a)
  ->IO()
connectAndDo action = do
  dbh <- connectPostgreSQL "dbname=pfdb port=5432 password=carcasse connect_timeout=3"
  action dbh
  disconnect dbh

-- | exécute une requête sql
executeQuery
  :: Connection       -- ^ connection courante
  -> String           -- ^ requête à exécuter
  -> [SqlValue]       -- ^ valeurs
  -> IO()

executeQuery dbh query values = handleSqlError $ do 
  sth <- prepare dbh query
  debugM logbase (query ++ " with values = " ++ (show values))
  execute sth values
  commit dbh

-- | retourne la prochaine valeur de la séquence
getNextSequenceValue
  :: Connection       -- ^ connection courante
  -> String           -- ^ nom de la séquence
  -> IO(SqlValue)     -- ^ valeur trouvée

getNextSequenceValue dbh sequence_name = handleSqlError $ do
  sth <- prepare dbh $ "SELECT nextval('" ++ sequence_name ++ "')" 
  execute sth []
  row <- fetchRow sth 
  finish sth
  case row of Just (folderid:_) -> return folderid
              _                 -> ioError $ userError ("error on getting nextval of "  ++ sequence_name)

-- | convertit une date en SqlValue pour la base de donnée.
toSqlTimeStamp
  :: CalendarTime   -- ^ la date
  -> SqlValue       -- ^ la valeur sql

toSqlTimeStamp calendar = toSql $ formatCalendarTime defaultTimeLocale "%Y-%b-%d %H:%M:%S" calendar

-- | désencapsule un maybe SqlValue
-- renvoit un SqlNull si le paramètre est Nothing
toSqlMaybe
  :: Maybe SqlValue   -- ^ un Maybe encapsulant une SqlValue
  -> SqlValue         -- ^ renvoit la valeur encapsulée ou SqlNull

toSqlMaybe (Just val) = val
toSqlMaybe Nothing    = SqlNull

