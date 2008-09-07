
module Main where

import System.Environment (getArgs)
import CtDataOperations
import CtDataAccess
import Database.HDBC 
import Database.HDBC.PostgreSQL 
import System.Console.GetOpt
import System.Log.Logger
import System.Log.Handler.Simple

data Options = Options 
  { optMinSize :: Maybe Integer
  , optMaxSize :: Maybe Integer
  , optIgnoreCase :: Bool
  } deriving Show

defaultOptions = Options
  { optMinSize = Nothing
  , optMaxSize = Nothing
  , optIgnoreCase = True 
  }

options :: [OptDescr(Options -> Options)]
options = [ Option ['c'] ["case sensitive"] (NoArg (\ o ->
      o{optIgnoreCase = False}))   "ignore case"
          , Option ['m'] ["minsize"] (OptArg (\f o -> case f of (Just x) -> o {optMinSize = Just (read x)}
                                                                Nothing  -> o ) "") "min size"
          , Option ['M'] ["maxsize"] (OptArg (\f o -> case f of (Just x) -> o {optMaxSize = Just (read x)}
                                                                Nothing  -> o ) "") "max size"
           ]

parseOptions args = case getOpt Permute options args of
  (o, [n], []) -> return (foldl (flip id) defaultOptions o,n)
  (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ctsearch [option] criteria"
  

main :: IO ()
main = do 
  fh <- fileHandler "out_search.log" DEBUG
  updateGlobalLogger rootLoggerName (setLevel INFO . setHandlers [fh])
  (options, criteria) <- getArgs >>= parseOptions
  connectAndDo (search criteria (optIgnoreCase options) (optMinSize options) (optMaxSize options))

