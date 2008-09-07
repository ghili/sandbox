
module CtDataOperations(recordFolderTree, search) where

import CtFileBrowsing
import CtDataAccess
import Data.Tree 
import Database.HDBC 
import Database.HDBC.PostgreSQL 
import System.Time
import System.Locale (defaultTimeLocale)
import System.FilePath(dropDrive, dropExtension, takeExtension)
import Text.Printf

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
  show SearchResultError                                         = "SearchResultError"

-- | effectue une recherche sur la base des fichiers
search
  :: String         -- ^ critère de recherche
  -> Bool           -- ^ ignore case
  -> Maybe Integer  -- ^ taille minimum
  -> Maybe Integer  -- ^ taille maximum
  -> Connection     -- ^ connection courante
  -> IO ()
search criteria ic minsize maxsize dbh = handleSqlError $ do
  putStrLn "fichiers trouves:"
  fileResults <- searchFile criteria ic minsize maxsize dbh
  putStrLn $ foldr ((++) . (++ "\n") . show) "" fileResults
  putStrLn "dossiers trouves :"
  folderResults <- searchFolder criteria ic minsize maxsize dbh
  putStrLn $ foldr ((++) . (++ "\n") . show) "" folderResults

searchFolder criteria ic minsize maxsize dbh = handleSqlError $ do
  searchQuery dbh "SELECT s.nom, d.chemin FROM ct.dossier as d JOIN ct.support as s ON (d.idsupport=s.id) WHERE d.nom " criteria ic populateFolderResult
  where populateFolderResult (support:(folderpath:_)) = FolderResult {srsupport = fromSql support, srfolderpath= fromSql folderpath}
        populateFolderResult _ = SearchResultError

searchFile criteria ic minsize maxsize dbh = handleSqlError $ do
  searchQuery dbh "SELECT f.nom, f.extension, f.taille, f.dtfichier, s.nom, d.chemin FROM ct.fichier as f JOIN ct.dossier as d ON (f.iddossier=d.id) JOIN ct.support as s ON (d.idsupport=s.id) WHERE f.nom " criteria ic populateFileResult
  where populateFileResult (filename:(extension:(size:(dtfichier:(support:(folderpath:_)))))) = FileResult {srfilename = (fromSql filename) ++ fromSql extension, srsize = fromSql size, srdtfichier = fromSql dtfichier,srsupport = fromSql support, srfolderpath= fromSql folderpath}
        populateFileResult _ = SearchResultError


searchQuery
  :: Connection
  -> String
  -> String
  -> Bool
  -> ([SqlValue] -> SearchResult)
  -> IO([SearchResult])
searchQuery dbh query criteria ic populate = do 
  sth <- prepare dbh (query ++ (case ic of True   -> " ILIKE ?"
                                           False  -> " LIKE ?" ))
  execute sth [toSql ("%" ++ criteria ++ "%")]
  rows <- fetchAllRows sth
  finish sth
  return $ map populate rows

