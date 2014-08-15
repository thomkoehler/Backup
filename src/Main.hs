
-----------------------------------------------------------------------------------------------------------------------

module Main(main) where

import System.Environment(getArgs)
import Text.Printf(printf)
import Control.Monad(forM_)
import Control.Lens

import Options
import BackupSuite
import DirScanner
import Compressor

-----------------------------------------------------------------------------------------------------------------------

main :: IO()
main = do
   argv <- getArgs
   opts <- getOptions argv
   suites <- decodeFile $ optInput opts
   forM_ suites doBackupSuite 
   return ()
   

doBackupSuite :: BackupSuite -> IO ()
doBackupSuite suite = do
   let targetDir = suite ^. bsDir
   forM_ (suite ^. bsBackups) $ doBackup targetDir 


doBackup :: FilePath -> Backup -> IO ()
doBackup targetDir backup = 
   if backup ^. bEnabled
      then do
         fileList <- list backup
         if null fileList 
            then 
               printf "Backup %s is empty." $ backup ^. bName
            else
               compress fileList (targetDir ++ "/" ++ (backup ^. bName))
      else
         printf "Backup %s is diabled." $ backup ^. bName       


-----------------------------------------------------------------------------------------------------------------------