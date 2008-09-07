module QCheckCtFileBrowsing
where

import CtFileBrowsing
import Test.QuickCheck
import System.Time
import System.IO.Unsafe

instance Arbitrary FileInfo where
    arbitrary = do 
      absoluteName  <- elements []
      name <- elements ["aze", "bidon", "truc"]
      size <- elements [0..10]
      return FileInfo {absoluteName= absoluteName, fileName=name, size = size, time =  unsafePerformIO $ getClockTime >>= toCalendarTime}

instance Arbitrary FolderInfo where
    arbitrary = do 
      folderName <- elements ["test", "touille"]
      folderPath <- elements ["c:/truc", "c:/machin"]
      return FolderInfo {folderName = folderName, folderPath = folderPath, files = [])
   
prop_ShowFolder file = show FolderInfo {folderName= "test", folderPath= "c:/truc/", files= [file]} == "c:/truc/test/" ++ fileName file
                           where types=(file::FileInfo)