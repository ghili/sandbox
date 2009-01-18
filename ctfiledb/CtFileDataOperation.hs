
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
      b_iddossier:: Maybe SqlValue,
      b_idsupport:: SqlValue,
      nom::String,
      rootPath::String,
      b_chemin::String,
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

recordFileNodes :: [String] -> StateWithIO BrowseState ()
recordFileNodes nomFichiers = do
  s <- get 
  liftIO $ do stmt <- (prepare (c s) insert_fichier)
              filesInfos <- (mapM (getFileInfo (b_chemin s)) nomFichiers)
              executeMany stmt $ getFilesInfoSqlValues (toSqlMaybe $ b_iddossier s) filesInfos

recordSupport :: String -> String -> StateWithIO BrowseState ()
recordSupport chemin label = do
  s <- get
  supportId <- liftIO $ do supportid <- getNextSequenceValue (c s) sequence_support
                           st <- prepare (c s) insert_support
                           execute st [supportid, SqlString label, SqlInteger( 0 )]
                           return supportid
  put s{b_idsupport = supportId, rootPath = chemin}
  recordFolder "/"

recordFolder :: String -> StateWithIO BrowseState ()
recordFolder nomDossier = do
  s <- get
  idDossier <- liftIO $ do folderid <- getNextSequenceValue (c s) sequence_dossier
                           st <- prepare (c s) insert_dossier
                           execute st [folderid, SqlString nomDossier, toSql (dropRootPath (b_chemin s) (rootPath s)), toSqlMaybe (b_iddossier s), b_idsupport s]
                           return $ Just folderid
  put s{b_iddossier = idDossier, b_chemin = msum [b_chemin s, "/", nomDossier]}
  tuple <- liftIO $ do contents <- getDirectoryContents (b_chemin s)
                       filteredContents <- return (dropWhile (\x -> x== "." || x== "..") contents)
                       paths <- return $ map (normalise . (combine (b_chemin s))) filteredContents
                       files <- filterM ((liftM not) . doesDirectoryExist) paths
                       dirs <- filterM doesDirectoryExist paths
                       return (files,dirs)
  recordFileNodes (fst tuple)
  sequence_ $ map recordFolder (snd tuple)
  where dropRootPath path rootPath = drop (length rootPath) path                     

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
