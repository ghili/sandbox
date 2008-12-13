
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
      b_iddossier:: IO (Maybe SqlValue),
      b_idsupport:: IO SqlValue,
      nom::String,
      rootPath::String,
      b_chemin::IO String,
      c::Connection
}

data FileInfo = FileInfo {
    absoluteName :: String,
    fileName :: String,
    size :: Integer,
    time :: CalendarTime 
} | NoAccess String

data FolderInfo = FolderInfo { folderName :: String, folderPath :: String, files :: [FileInfo]}


{--recordFileNode :: IO String -> State BrowseState (IO Integer)
recordFileNode nomFichier = do
  s <- get
  return $ do st <- prepare (c s) "INSERT INTO ct.fichier (id_dossier, nom, extension, taille, date_fichier) VALUES (?, ?, ?, ?, ?)"
              f <- nomFichier
              fileInfo <- getFileInfo (b_chemin s) f
              idDossier <- b_iddossier s
              execute st $ getFileInfoSqlValues (toSqlMaybe idDossier) fileInfo --}

recordFileNodes :: IO [String] -> State BrowseState (IO ())
recordFileNodes nomFichiers = do
  s <- get
  return $ do st <- prepare (c s) "INSERT INTO ct.fichier (id_dossier, nom, extension, taille, date_fichier) VALUES (?, ?, ?, ?, ?)"
              f <- nomFichiers
              filesInfos <- mapM (getFileInfo (b_chemin s)) f
              idDossier <- b_iddossier s
              executeMany st $ getFilesInfoSqlValues (toSqlMaybe idDossier) filesInfos

recordSupport :: String -> String -> State BrowseState ()
recordSupport chemin label = do
  s <- get
  supportId <- return $ do supportid <- getNextSequenceValue (c s) "ct.support_id_support_seq"
                           st <- prepare (c s) "INSERT INTO ct.support (id_support, nom, chck, date_creation) VALUES (?, ?, ?, LOCALTIMESTAMP)"
                           execute st [supportid, SqlString label, SqlInteger( 0 )]
                           return supportid
  put s{b_idsupport = supportId, rootPath = chemin}
  recordFolder "/"

recordFolder :: String -> State BrowseState ()
recordFolder nomDossier = do
  s <- get
  idDossier <- return $ do folderid <- getNextSequenceValue (c s) "ct.dossier_id_dossier_seq" 
                           st <- prepare (c s) "INSERT INTO ct.dossier (id_dossier, nom, chemin, id_dossier_parent, id_support) VALUES (?, ?, ?, ?, ?)"
                           idDossier <- b_iddossier s
                           chemin <- b_chemin s
                           supportId <- b_idsupport s
                           execute st [folderid, SqlString nomDossier, toSql (dropRootPath chemin (rootPath s)), toSqlMaybe idDossier, supportId]
                           return $ Just folderid
  put s{b_iddossier = idDossier, b_chemin = msum [(b_chemin s), return "/", return nomDossier]}
  tuple <- return $ do chemin <- b_chemin s
                       contents <- getDirectoryContents chemin
                       filteredContents <- return (dropWhile (\x -> x== "." || x== "..") contents)
                       paths <- return $ map (normalise . (combine chemin)) filteredContents
                       files <- filterM ((liftM not) . doesDirectoryExist) paths
                       dirs <- filterM doesDirectoryExist paths
                       return (files,dirs)
  recordFileNodes ((liftM fst) tuple)
  sequence_ $ map recordFolder (snd $ unsafePerformIO tuple)
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
  :: IO String -- ^ le chemin relatif
  -> String -- ^ le nom du fichier
  -> IO FileInfo -- ^ la structure renvoyée contenant les informations sur le fichier
getFileInfo path file = do
  catch (do
    chemin <- path
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
