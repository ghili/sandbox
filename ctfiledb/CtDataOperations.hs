
module CtDataOperations(search, SearchCriteria(..)) where

import CtDataAccess
import Data.Tree 
import Database.HDBC 
import Database.HDBC.PostgreSQL 
import System.Time
import System.Locale (defaultTimeLocale)
import System.FilePath(dropDrive, dropExtension, takeExtension)
import System.Log.Logger
import Text.Printf
import Data.Maybe

logbase = "CtDataOperations"

data SearchCriteria = SearchCriteria {
      keyword :: String,
      ignoreCase :: Bool,
      minSize :: Maybe Integer,
      maxSize :: Maybe Integer
} deriving Show

data SearchResult = FileResult {
  srfilename    :: String,
  srsize        :: Integer,
  srdtfichier   :: CalendarTime,
  srsupport     :: String,
  srfolderpath  :: String
} | FolderResult {
  srsupport     :: String,
  srfolderpath  :: String
} | SearchResultError 

instance Show(SearchResult) where
  show (FileResult filename size dtfichier support folderpath) = 
    (printf "%-20s %10ib  %15s" filename size (formatCalendarTime defaultTimeLocale "%d/%m/%C %H:%M:%S" dtfichier)) ++ " in " ++ support ++ "@" ++ folderpath
  show (FolderResult support folderpath) = support ++ "@" ++ folderpath
  show SearchResultError                                       = "SearchResultError"

-- | effectue une recherche sur la base des fichiers
search :: SearchCriteria -> Connection -> IO ()
search criteria dbh = handleSqlError $ do
  debugM logbase $ show criteria
  putStrLn "fichiers trouves:"
  fileResults <- searchFile criteria dbh
  putStrLn $ foldr ((++) . (++ "\n") . show) "" fileResults
  putStrLn "dossiers trouves :"
  folderResults <- searchFolder criteria dbh
  putStrLn $ foldr ((++) . (++ "\n") . show) "" folderResults

searchFolder criteria dbh = handleSqlError $ do
  searchQuery dbh query  criteria populateFolderResult
  where criterion = addKeywordCriteriaWithIgnoreCase [] criteria "d.nom"
        query = "SELECT s.nom, d.chemin FROM ct.dossier as d JOIN ct.support as s ON (d.id_support=s.id_support) WHERE " ++ (mkString criterion " AND ")
        populateFolderResult (support:(folderpath:_)) = FolderResult {srsupport = fromSql support, srfolderpath= fromSql folderpath}
        populateFolderResult _ = SearchResultError

searchFile criteria  dbh = handleSqlError $ do
  searchQuery dbh query criteria populateFileResult
  where criterion = addSizeCriteria (addKeywordCriteriaWithIgnoreCase [] criteria "f.nom") criteria
        query = "SELECT f.nom, f.extension, f.taille, f.date_fichier, s.nom, d.chemin FROM ct.fichier as f JOIN ct.dossier as d ON (f.id_dossier=d.id_dossier) JOIN ct.support as s ON (d.id_support=s.id_support) WHERE " ++ (mkString criterion " AND ")
        populateFileResult (filename:(extension:(size:(dtfichier:(support:(folderpath:_)))))) = FileResult {srfilename = (fromSql filename) ++ fromSql extension, srsize = fromSql size, srdtfichier = fromSql dtfichier,srsupport = fromSql support, srfolderpath= fromSql folderpath}
        populateFileResult _ = SearchResultError


addKeywordCriteriaWithIgnoreCase::[String] -> SearchCriteria -> String -> [String]
addKeywordCriteriaWithIgnoreCase criterion criteria alias = 
  (alias ++ (case ignoreCase criteria of True   -> " ILIKE ? "
                                         False  -> " LIKE ? " )) : criterion

--TODO ajouter les valeurs dans une autre liste pour passer à la méthode execute
--ajoute les critères sur la taille min et max si présents
addSizeCriteria::[String] -> SearchCriteria -> [String]
addSizeCriteria criterion criteria =  
    criterion ++ ((maybeToList $ minSize criteria) >>= \x -> [" f.taille > "++ show x]) ++ ((maybeToList $ maxSize criteria) >>= \x -> [" f.taille < "++ show x])

mkString (x:xs) sep = x ++ foldl (++) [] (addSep xs sep)
    where addSep (a:as) sep = (sep ++ a) : addSep as sep  
          addSep [] sep = []

searchQuery :: Connection -> String -> SearchCriteria -> ([SqlValue] -> SearchResult) -> IO([SearchResult])
searchQuery dbh query criteria populate = do 
  debugM logbase query
  sth <- prepare dbh query 
  execute sth [toSql ("%" ++ (keyword criteria) ++ "%")]
  rows <- fetchAllRows sth
--  finish sth
  return $ map populate rows

