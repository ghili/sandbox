
module CtFileDataOperations
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
import System.IO.Unsafe(unsafePerformIO)


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

recordFileNodes :: [String] -> BrowseState -> IO()
recordFileNodes nomFichiers s = do
  do stmt <- (prepare (c s) insert_fichier)
     filesInfos <- (mapM (getFileInfo (chemin s)) nomFichiers)
     executeMany stmt $ getFilesInfoSqlValues (toSqlMaybe $ iddossier s) filesInfos

recordSupport :: String -> String -> StateWithIO BrowseState ()
recordSupport chemin label = do
  s <- get
  supportId <- liftIO $ do supportid <- getNextSequenceValue (c s) sequence_support
                           st <- prepare (c s) insert_support
                           execute st [supportid, SqlString label, SqlInteger( 0 )]
                           return supportid
  put s{idsupport = supportId, rootPath = chemin}
  recordFolder "/"

recordFolder :: String -> StateWithIO BrowseState ()
recordFolder nomDossier = do
  s <- get
  idDossier <- liftIO $ do folderid <- getNextSequenceValue (c s) sequence_dossier
                           st <- prepare (c s) insert_dossier
                           execute st [folderid, SqlString nomDossier, toSql (dropRootPath s), toSqlMaybe (iddossier s), idsupport s]
                           return $ Just folderid
  put s{iddossier = idDossier, chemin = msum [chemin s, "/", nomDossier]}
  folders <- liftIO $ do contents <- getDirectoryContents (chemin s)
                         filteredContents <- return (dropWhile (\x -> x== "." || x== "..") contents)
                         paths <- return $ map (normalise . (combine (chemin s))) filteredContents
                         files <- filterM ((liftM not) . doesDirectoryExist) paths
                         recordFileNodes files s
                         filterM doesDirectoryExist paths
  sequence_ $ map recordFolder folders
  where dropRootPath s = drop (length (rootPath s)) (chemin s)

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

-- | construit les arbres de chaque dossier pass� en param�tre
browseEachFolder 
  :: [String] -- ^ une liste de chemin vers les dossiers demand�s
  -> IO [Tree FolderInfo] -- ^ liste d'arbres retourn�e
browseEachFolder l = foldr ((liftM2 (:)) . (browseFolder2 "" ) . joinPath . splitDirectories . dropTrailingPathSeparator) (return [])  l

--}

-- | retourne des informations sur un fichier
getFileInfo 
  :: String -- ^ le chemin relatif
  -> String -- ^ le nom du fichier
  -> IO FileInfo -- ^ la structure renvoy�e contenant les informations sur le fichier
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
