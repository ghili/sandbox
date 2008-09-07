

module Main where

import System.Environment (getArgs)
import CtFileBrowsing
import CtDataOperations
import CtDataAccess
import Data.Tree
import System.Console.GetOpt
import System.Log.Logger
import System.Log.Handler.Simple
import Database.HDBC.PostgreSQL 

data Flag = 
  SupportNameFlag String
  deriving Show

options :: [OptDescr Flag]
options = [ Option ['l'] ["label"] (ReqArg (\x->SupportNameFlag x) "")    "label to give" ]

parseOptions args = case getOpt Permute options args of
  ([SupportNameFlag o], [n], []) -> return (o,n)
  (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: -l label directory"
  

main :: IO ()
main = do 
  fh <- fileHandler "out_inject.log" DEBUG
  updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [fh])
  (targetlabel, folderpath) <- getArgs >>= parseOptions
  folderForest <- browseEachFolder [folderpath] 
  mapM_ (connectAndDo . (recordFolderTree targetlabel)) folderForest
  debugM rootLoggerName (foldr ((++) . (++ "\n") . show . flatten) "" folderForest)



