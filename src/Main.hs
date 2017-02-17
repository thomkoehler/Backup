
-----------------------------------------------------------------------------------------------------------------------

module Main(main) where

import System.Environment(getArgs)
import Text.Printf(printf)
import Control.Monad(forM_)
import Control.Lens
import Data.Time

import Options
import BackupSuite
import DirScanner
import Compressor
import Version

-----------------------------------------------------------------------------------------------------------------------

main :: IO()
main = do
   argv <- getArgs
   opts <- getOptions argv
   select
      [
         optHelp opts,
         optShowVersion opts
      ]
      [
         printUsage,
         putStrLn version
      ]
      $ do
         suites <- decodeFile $ optInput opts
         forM_ suites $ doBackupSuite opts


doBackupSuite :: Options -> BackupSuite -> IO ()
doBackupSuite options suite = do
   let targetDir = suite ^. bsDir
   currTime <- currentTimeStr
   forM_ (loockupBackups (optBackupName options) suite) $ doBackup options targetDir currTime


doBackup :: Options -> FilePath -> String -> Backup -> IO ()
doBackup options targetDir currTime backup =
   if backup ^. bEnabled
      then do
         fileList <- list backup
         if null fileList
            then
               printf "Backup %s is empty." $ backup ^. bName
            else
               compress options fileList (targetDir ++ "/" ++ (backup ^. bName) ++ currTime) $ backup ^. bPassword
      else
         printf "Backup %s is diabled." $ backup ^. bName


currentTimeStr :: IO String
currentTimeStr = do
   time <- getZonedTime
   return $ formatTime defaultTimeLocale "_%y%m%d_%H%M%S" time


select :: [Bool] -> [a] -> a -> a
select [] _ def = def
select _ [] def = def
select (b:bs) (x:xs) def = if b
   then x
   else select bs xs def

-----------------------------------------------------------------------------------------------------------------------
