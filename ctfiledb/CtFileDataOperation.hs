
module CtFileDataOperation
where

import Database.HDBC 
import Database.HDBC.PostgreSQL 
import CtDataAccess
import System.Directory (getModificationTime, getDirectoryContents, doesDirectoryExist)
import System.FilePath (joinPath, splitDirectories,dropDrive, dropExtension, takeExtension,
    pathSeparator,normalise, combine, dropTrailingPathSeparator)
import System.IO
import Control.Monad
import Data.Tree
import System.Time (CalendarTime, formatCalendarTime, toCalendarTime)
import System.Locale (defaultTimeLocale)
import Control.Monad.State
import Text.Printf
import Data.Maybe
import System.Log.Logger

logbase = "CtFileDataOperation"

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

data FolderInfo = FolderInfo { folderName :: String, folderPath :: String, files :: [FileInfo]}

insert_fichier = "INSERT INTO ct.fichier (id_dossier, nom, extension, taille, date_fichier) VALUES (?, ?, ?, ?, ?)"
insert_support = "INSERT INTO ct.support (id_support, nom, chck, date_creation) VALUES (?, ?, ?, LOCALTIMESTAMP)"
insert_dossier = "INSERT INTO ct.dossier (id_dossier, nom, chemin, id_dossier_parent, id_support) VALUES (?, ?, ?, ?, ?)"
sequence_support = "ct.support_id_support_seq"
sequence_dossier = "ct.dossier_id_dossier_seq" 


execRecordSupport :: String -> String -> Connection -> IO BrowseState
execRecordSupport chemin label dbh =
    execStateT (recordSupport chemin label) $ BrowseState{iddossier = Nothing, idsupport = SqlNull, nom = label , rootPath=chemin, chemin = chemin, c = dbh}

recordFileNodes :: [String] -> BrowseState -> IO()
recordFileNodes nomFichiers s = do
   stmt <- (prepare (c s) insert_fichier)
   filesInfos <- (mapM (getFileInfo (chemin s)) nomFichiers)
   debugM logbase $ "record files" ++ (show nomFichiers)
   executeMany stmt $ getFilesInfoSqlValues (toSqlMaybe $ iddossier s) filesInfos

recordSupport :: String -> String -> StateWithIO BrowseState ()
recordSupport chemin label = do
  s <- get
  supportId <- liftIO $ do supportid <- getNextSequenceValue (c s) sequence_support
                           st <- prepare (c s) insert_support
                           execute st [supportid, SqlString label, SqlInteger( 0 )]
                           return supportid
  put s{idsupport = supportId, rootPath = chemin}
  recordFolder ""

recordFolder :: String -> StateWithIO BrowseState ()
recordFolder nomDossier = do
  s <- get
  idDossier <- liftIO $ do folderid <- getNextSequenceValue (c s) sequence_dossier
                           st <- prepare (c s) insert_dossier
                           debugM logbase $ "record folder" ++ nomDossier
                           execute st [folderid, SqlString nomDossier, toSql (dropRootPath s), toSqlMaybe (iddossier s), idsupport s]
                           return $ Just folderid
  put s{iddossier = idDossier, chemin = combine (chemin s) nomDossier}
  s <- get
  res <- liftIO $ do 
    contents <- (liftM filterNotDots) $ getDirectoryContents (chemin s)
    files <- filterM ((liftM not) . (doesDirectoryNameExist s)) contents
    recordFileNodes files s
    folders <- filterM (doesDirectoryNameExist s) contents
    mapM_ (\f->evalStateT (recordFolder f) s) folders
  return res
  where dropRootPath s = drop (length (rootPath s)) (chemin s)
        filterNotDots = filter (\p -> p /= "." && p /="..")
        fullPath s relativePath = normalise $ combine (chemin s) relativePath
        doesDirectoryNameExist s dirName = doesDirectoryExist(fullPath s dirName)

{--
instance Show (FileInfo) where
    show (FileInfo absoluteName fileName size calendar) =
                        show fileName ++ "\t" ++ show size ++ " bytes" ++ "\t" 
                             ++ (formatCalendarTime defaultTimeLocale "%d %b %Y %H:%M" calendar)
    show (NoAccess x)                                   = x

instance Show (FolderInfo) where
    show (FolderInfo folderName folderPath files) = 
                        "\n" ++ show folderName ++ " :\n" ++ (foldr ((++) . (++ "\n") . show) "" files)

browseFolder2 :: String -> String -> IO (Tree FolderInfo)
browseFolder2 fname fpath = browseFolder fname fpath fpath

-- | construit les arbres de chaque dossier passé en paramètre
browseEachFolder 
  :: [String] -- ^ une liste de chemin vers les dossiers demandés
  -> IO [Tree FolderInfo] -- ^ liste d'arbres retournée
browseEachFolder l = foldr ((liftM2 (:)) . (browseFolder2 "" ) . joinPath . splitDirectories . dropTrailingPathSeparator) (return [])  l

--}

-- | retourne des informations sur un fichier
getFileInfo 
  :: String -- ^ le chemin relatif
  -> String -- ^ le nom du fichier
  -> IO FileInfo -- ^ la structure renvoyée contenant les informations sur le fichier
getFileInfo chemin file = do
  catch (do
    h <- openFile chemin ReadMode 
    size <- hFileSize h
    time <- getModificationTime chemin
    calendar <- toCalendarTime time
    return $ FileInfo {absoluteName= chemin, fileName = file, size=size, time=calendar })
        (\error -> return $ NoAccess (show error))

-- | transforme les informations sur les fichiers en valeurs sql avec  l'id du dossier
getFilesInfoSqlValues :: SqlValue  -> [FileInfo]  -> [[SqlValue]]
getFilesInfoSqlValues folderid files   = let fileInfoToSql (FileInfo absoluteName fileName size calendar) = [SqlString (dropExtension fileName), SqlString (takeExtension fileName), SqlInteger size, toSqlTimeStamp calendar , folderid]
                                             gotAccess (NoAccess msg) = False
                                             gotAccess _ = True
                                           in map fileInfoToSql (filter gotAccess files)

getFileInfoSqlValues :: SqlValue  -> FileInfo  -> [SqlValue]
getFileInfoSqlValues folderid fileInfo   = let fileInfoToSql (FileInfo absoluteName fileName size calendar) = [SqlString (dropExtension fileName), SqlString (takeExtension fileName), SqlInteger size, toSqlTimeStamp calendar , folderid]
                                               fileInfoToSql (NoAccess msg) = []
                                           in fileInfoToSql fileInfo
