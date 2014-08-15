
-----------------------------------------------------------------------------------------------------------------------

module Compressor(compress) where

import System.IO.Temp(withSystemTempFile)
import System.IO(hPutStrLn, hClose)
import System.Directory(doesFileExist)
import Control.Monad(forM_, when)
import System.Process(system)
import System.Exit(ExitCode(..))
import Text.Printf(printf)

-----------------------------------------------------------------------------------------------------------------------

compress  :: [String] -> String -> IO ()
compress = compress_rar 


compress_rar :: [String] -> String -> IO ()
compress_rar fileList archiveName = do
   let properAchiveName = archiveName ++ ".rar"
   archiveExists <- doesFileExist properAchiveName
   when archiveExists $ error $ printf "Archive \"%s\" allready exists." properAchiveName  
   withListFile fileList $ \fileListName -> do
      let cmd = printf "rar a \"%s\" \"@%s\" " properAchiveName fileListName
      system' cmd


withListFile :: [String] -> (FilePath -> IO a) -> IO a
withListFile fileList listFun = 
   withSystemTempFile "Backup" $ \filePath handle -> do
      forM_ fileList (hPutStrLn handle)
      hClose handle
      listFun filePath
      
      
system' :: String -> IO ()
system' cmd = do
   res <- system cmd
   case res of
      ExitSuccess   -> return ()
      ExitFailure _ -> error $ printf "Command %s failed." cmd

-----------------------------------------------------------------------------------------------------------------------
