
module CtFileBrowsing (
        FileInfo(..), FolderInfo(..),
        browseEachFolder)
where

import System.Directory (getModificationTime, getDirectoryContents, doesDirectoryExist)
import System.FilePath (joinPath, splitDirectories,
    pathSeparator,normalise, combine, dropTrailingPathSeparator)
import System.IO
import Control.Monad
import Data.Tree
import System.Time (CalendarTime, formatCalendarTime, toCalendarTime)
import System.Locale (defaultTimeLocale)

data FileInfo = FileInfo {
    absoluteName :: String,
    fileName :: String,
    size :: Integer,
    time :: CalendarTime 
} | NoAccess String

data FolderInfo = FolderInfo { folderName :: String, folderPath :: String, files :: [FileInfo]}

instance Show (FileInfo) where
    show (FileInfo absoluteName fileName size calendar) =
                        show fileName ++ "\t" ++ show size ++ " bytes" ++ "\t" 
                             ++ (formatCalendarTime defaultTimeLocale "%d %b %Y %H:%M" calendar)
    show (NoAccess x)                                   = x

instance Show (FolderInfo) where
    show (FolderInfo folderName folderPath files) = 
                        "\n" ++ show folderName ++ " :\n" ++ (foldr ((++) . (++ "\n") . show) "" files)

-- | construit un arbre � partir de l'arborescence du syst�me de fichiers
-- en dessous du dossier renseign�
browseFolder 
  :: String -- ^ le nom du r�pertoire � parcourir
  -> String -- ^ le chemin relatif du r�pertoire � parcourir
  -> String -- ^ le chemin du r�pertoire racine
  -> IO (Tree FolderInfo) -- ^ l'arbre retourn�
browseFolder fname fpath rootPath = do
  contents <- getDirectoryContents fpath 
  infos <- getFilesFolders contents
  return Node { rootLabel = FolderInfo {folderName = fname, folderPath = dropRootPath fpath rootPath, files = fst infos}, subForest =  snd infos}
  where getFilesFolders contents =
          foldr (\xn z -> 
                  let path = normalise $ combine fpath xn
                  in do
                    isDir <- doesDirectoryExist path
                    tuple <- z
                    if isDir 
                      then do info <- browseFolder xn path rootPath
                              return (fst tuple , info:(snd tuple))
                      else do info <- getFileInfo xn path
                              return (info:(fst tuple),snd tuple))
                (return ([],[])) (dropWhile (\x -> x == "." || x == ".." ) contents)
        dropRootPath path rootPath = drop (length rootPath) path 

browseFolder2 :: String -> String -> IO (Tree FolderInfo)
browseFolder2 fname fpath = browseFolder fname fpath fpath

-- | construit les arbres de chaque dossier pass� en param�tre
browseEachFolder 
  :: [String] -- ^ une liste de chemin vers les dossiers demand�s
  -> IO [Tree FolderInfo] -- ^ liste d'arbres retourn�e
browseEachFolder l = foldr ((liftM2 (:)) . (browseFolder2 "" ) . joinPath . splitDirectories . dropTrailingPathSeparator) (return [])  l

-- | retourne des informations sur un fichier
getFileInfo 
  :: String -- ^ le nom du fichier
  -> String -- ^ le chemin relatif
  -> IO FileInfo -- ^ la structure renvoy�e contenant les informations sur le fichier
getFileInfo file path = do
  catch (do
    h <- openFile path ReadMode 
    size <- hFileSize h
    time <- getModificationTime path
    calendar <- toCalendarTime time
    return $ FileInfo {absoluteName= path, fileName = file, size=size, time=calendar })
        (\error -> return $ NoAccess (show error))

