
module CtDataOperations(recordFolderTree, search, SearchCriteria(..)) where

import CtFileBrowsing
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

-- | ouvre une connection à la base de donnée puis insère les données contenues
-- dans l'arbre.
recordFolderTree
  :: String          -- ^ nom du support à donner
  -> Tree FolderInfo -- ^ les données à insérer
  -> Connection      -- ^ connection courante
  -> IO()

recordFolderTree name folderTree dbh = do
  supportid <- insertSupportInfo dbh name folderTree
  insertTree dbh Nothing supportid folderTree
  
-- | insère les données contenues dans l'arbre.
insertTree
  :: Connection           -- ^ connection courante
  -> Maybe SqlValue       -- ^ id du dossier parent
  -> SqlValue             -- ^ id du support 
  -> Tree FolderInfo      -- ^ donnée à insérer 
  -> IO ()

insertTree dbh parentid supportid (Node folderInfo subForest)= do
  folderid <- insertFolderInfo dbh parentid supportid folderInfo
  mapM_ (insertTree dbh folderid supportid) subForest

-- | insère les informations du dossier et de ses fichiers en base.
insertFolderInfo
  :: Connection             -- ^ connection courante
  -> Maybe SqlValue         -- ^ id du dossier parent
  -> SqlValue               -- ^ id du support
  -> FolderInfo             -- ^ infos du dossier
  -> IO (Maybe SqlValue)    -- ^ id du dossier inséré

insertFolderInfo dbh parentid supportid (FolderInfo folderName folderPath files) = handleSqlError $ do
  folderid <- getNextSequenceValue dbh "ct.dossier_id_seq" 
  executeQuery dbh "INSERT INTO ct.dossier (id, nom, chemin, iddossierparent, idsupport) VALUES (?, ?, ?, ?, ?)" [folderid, SqlString folderName, toSql (dropDrive folderPath), toSqlMaybe parentid, supportid]
  mapM_ (insertFileInfo dbh folderid) files 
  return $ Just folderid

-- | insère les informations relatives à un fichier.
-- La fonction n'insère rien s'il n'y a pas d'accès aux informations du fichier.
insertFileInfo
  :: Connection -- ^ connection courante
  -> SqlValue   -- ^ id du dossier
  -> FileInfo   -- ^ informations relatives à un fichier
  -> IO()

insertFileInfo dbh folderid (FileInfo absoluteName fileName size calendar)  = 
  executeQuery dbh "INSERT INTO ct.fichier (nom, extension, taille, dtfichier, iddossier) VALUES (?, ?, ?, ?, ?)" [SqlString (dropExtension fileName), SqlString (takeExtension fileName), SqlInteger size, toSqlTimeStamp calendar , folderid]
insertFileInfo dbh folderid (NoAccess msg)                                  = return ()

-- | insère les informations sur le support
insertSupportInfo
  :: Connection       -- ^ connection courante
  -> String           -- ^ nom du support
  -> Tree FolderInfo  -- ^ arborescence de dossiers
  -> IO (SqlValue)    -- ^ id du support

insertSupportInfo dbh name tree = do
  supportid <- getNextSequenceValue dbh "ct.support_id_seq"
  executeQuery dbh "INSERT INTO ct.support (id, nom, chck, dtcreation) VALUES (?, ?, ?, LOCALTIMESTAMP)" [supportid, SqlString name, SqlInteger(checkFolder tree)]
  return supportid

checkFolder
  :: Tree FolderInfo
  -> Integer
checkFolder (Node (FolderInfo f p files) subForest) = (foldr ((+) . checkFolder) 0 subForest) + (checkFile files)

checkFile 
  :: [FileInfo]
  -> Integer
checkFile ((FileInfo a f size c):xs)               = size + (checkFile xs)
checkFile []                          = 0
checkFile _                          = 0



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

-- | TODO state monad
-- | effectue une recherche sur la base des fichiers
search
  :: SearchCriteria -- ^ critères de recherche
  -> Connection     -- ^ connection courante
  -> IO ()
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
        query = "SELECT s.nom, d.chemin FROM ct.dossier as d JOIN ct.support as s ON (d.idsupport=s.id) WHERE " ++ (mkString criterion " AND ")
        populateFolderResult (support:(folderpath:_)) = FolderResult {srsupport = fromSql support, srfolderpath= fromSql folderpath}
        populateFolderResult _ = SearchResultError

searchFile criteria  dbh = handleSqlError $ do
  searchQuery dbh query criteria populateFileResult
  where criterion = addSizeCriteria (addKeywordCriteriaWithIgnoreCase [] criteria "f.nom") criteria
        query = "SELECT f.nom, f.extension, f.taille, f.dtfichier, s.nom, d.chemin FROM ct.fichier as f JOIN ct.dossier as d ON (f.iddossier=d.id) JOIN ct.support as s ON (d.idsupport=s.id) WHERE " ++ (mkString criterion " AND ")
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

searchQuery
  :: Connection
  -> String
  -> SearchCriteria
  -> ([SqlValue] -> SearchResult)
  -> IO([SearchResult])
searchQuery dbh query criteria populate = do 
  debugM logbase query
  sth <- prepare dbh query 
  execute sth [toSql ("%" ++ (keyword criteria) ++ "%")]
  rows <- fetchAllRows sth
--  finish sth
  return $ map populate rows

