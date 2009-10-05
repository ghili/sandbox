
module CtFileDataOperation
where

import Database.HDBC 
import Database.HDBC.PostgreSQL 
import CtDataAccess
import System.Directory (getModificationTime, getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath (joinPath, splitDirectories,dropDrive, dropExtension, takeExtension,
    pathSeparator,normalise, combine, dropTrailingPathSeparator)
import System.IO
import Control.Monad
import System.Time (CalendarTime, formatCalendarTime, toCalendarTime)
import System.Locale (defaultTimeLocale)
import Control.Monad.State
import Text.Printf
import Data.Maybe
import System.Log.Logger
import Debug.Trace

-------------------

data BrowseState = BrowseState {
      iddossier:: Maybe SqlValue,
      idsupport:: SqlValue,
      nom::String,
      rootPath::String,
      chemin::String,
      c::Connection
}

type StateWithIO s a = StateT s IO a

data FileInfo = FileInfo {
    absoluteName :: String,
    fileName :: String,
    size :: Integer,
    time :: CalendarTime 
} | NoAccess String

instance Show (FileInfo) where
    show (FileInfo absoluteName fileName size calendar) =
                        show fileName ++ "\t" 
                        ++ show size ++ " bytes" ++ "\t" 
                        ++ (formatCalendarTime defaultTimeLocale "%d %b %Y %H:%M" calendar)
    show (NoAccess x) = x

data FolderInfo = FolderInfo {
      folderName :: String, 
      folderPath :: String, 
      files :: [FileInfo]
}

instance Show (FolderInfo) where
    show (FolderInfo folderName folderPath files) = 
                        "\n" ++ show folderName 
                           ++ " :\n" ++ (foldr ((++) . (++ "\n") . show) "" files)

-------------------

logbase = "CtFileDataOperation"

insert_fichier = "INSERT INTO ct.fichier (id_dossier, nom, extension, taille, date_fichier) VALUES (?, ?, ?, ?, ?)"
insert_support = "INSERT INTO ct.support (id_support, nom, chck, date_creation) VALUES (?, ?, ?, LOCALTIMESTAMP)"
insert_dossier = "INSERT INTO ct.dossier (id_dossier, nom, chemin, id_dossier_parent, id_support) VALUES (?, ?, ?, ?, ?)"
sequence_support = "ct.support_id_support_seq"
sequence_dossier = "ct.dossier_id_dossier_seq" 

-------------------

execRecordSupport :: String -> String -> Connection -> IO BrowseState
execRecordSupport chemin label dbh = 
    withTransaction dbh $ (\conn ->  
      execStateT (recordSupport chemin label) BrowseState{
                             iddossier = Nothing, 
                             idsupport = SqlNull, 
                             nom = label , 
                             rootPath = dropTrailingPathSeparator chemin, 
                             chemin = chemin, 
                             c = conn})

recordFileNodes :: Connection -> Maybe SqlValue -> [FileInfo] -> IO ()
recordFileNodes conn iddossier filesInfos = do
   stmt <- prepare conn insert_fichier
   handleSqlError $
     executeMany stmt $
       getFilesInfoSqlValues (toSqlMaybe iddossier) filesInfos

recordSupport :: String -> String -> StateWithIO BrowseState ()
recordSupport chemin label = do
  s <- get
  supportId <- liftIO $ do 
    supportid <- getNextSequenceValue (c s) sequence_support
    st <- prepare (c s) insert_support
    check <- checkFolder chemin
    handleSqlError $ execute st [supportid, SqlString label, SqlInteger check]
    return supportid
  put s{idsupport = supportId}
  recordFolder ""

recordFolder :: String -> StateWithIO BrowseState ()
recordFolder nomDossier = do
  s <- get
  idDossier <- liftIO $ do 
    folderid <- getNextSequenceValue (c s) sequence_dossier
    st <- prepare (c s) insert_dossier
    handleSqlError $ 
      execute st [folderid, 
                  SqlString nomDossier, 
                  toSql (dropRootPath s), 
                  toSqlMaybe (iddossier s), 
                  idsupport s]
    return $ Just folderid
  put s{iddossier = idDossier, chemin = combine (chemin s) nomDossier}
  s <- get
  res <- liftIO $ do 
    contents <- liftM filterNotDots $ getDirectoryContents (chemin s)
    files <- filterM (isFile s) contents
    filesInfos <- fileNamesToFileInfo (chemin s) files
    recordFileNodes (c s) (iddossier s) filesInfos
    folders <- filterM (doesDirectoryNameExist s) contents
    mapM_ (\f->evalStateT (recordFolder f) s) folders
  return res
  where dropRootPath s                   = drop (length (rootPath s) +1 ) (chemin s)
        fullPath s relativePath          = normalise $ combine (chemin s) relativePath
        isFile s fileName                = doesFileExist(fullPath s fileName)
        doesDirectoryNameExist s dirName = doesDirectoryExist(fullPath s dirName)

-- calcule la somme des tailles des fichiers d'un répertoire
checkFolder :: String -> IO Integer
checkFolder chemin = do
  contents <- liftM filterNotDots $ getDirectoryContents chemin
  paths <- return $ map (combine chemin) contents
  fileSizes <- filterM doesFileExist paths >>= mapM getFileSize 
  folderSizes <- filterM doesDirectoryExist paths >>= mapM checkFolder 
  return $ (sum fileSizes) + (sum folderSizes)
    where getFileSize chemin = do 
            h <- openFile chemin ReadMode 
            hFileSize h
  
filterNotDots = filter (\p -> p /= "." && p /="..")

fileNamesToFileInfo :: String -> [String] -> IO [FileInfo]
fileNamesToFileInfo chemin nomFichiers = mapM (fileNameToFileInfo chemin) nomFichiers

-- | retourne des informations sur un fichier
fileNameToFileInfo 
  :: String -- ^ le chemin relatif
  -> String -- ^ le nom du fichier
  -> IO FileInfo -- ^ la structure renvoyée contenant les informations sur le fichier
fileNameToFileInfo chemin file = 
    let filepath = combine chemin file in
    do 
      catch (do
           h <- trace filepath $ openFile filepath ReadMode 
           size <- hFileSize h
           time <- getModificationTime filepath
           calendar <- toCalendarTime time
           return $ FileInfo {absoluteName= filepath, 
                              fileName = file, 
                              size=size, 
                              time=calendar }
         )(\error -> return $ NoAccess (show error))

-- transforme les informations sur les fichiers en valeurs sql avec  l'id du dossier
getFilesInfoSqlValues :: SqlValue  -> [FileInfo]  -> [[SqlValue]]
getFilesInfoSqlValues folderid files   = 
    let gotAccess (NoAccess msg) = False
        gotAccess _ = True
    in map (getFileInfoSqlValues folderid) (filter gotAccess files)

-- transforme les informations d'un fichier
getFileInfoSqlValues :: SqlValue  -> FileInfo  -> [SqlValue]
getFileInfoSqlValues folderid (FileInfo absoluteName fileName size calendar) = 
    [folderid,
     SqlString (dropExtension fileName), 
     SqlString (takeExtension fileName), 
     SqlInteger size, 
     toSqlTimeStamp calendar]
getFileInfoSqlValues folderid (NoAccess msg)   =  []

