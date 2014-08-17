
-----------------------------------------------------------------------------------------------------------------------

module Compressor(compress) where

import System.IO.Temp(withSystemTempFile)
import System.IO(hPutStrLn, hClose)
import System.Directory(doesFileExist)
import Control.Monad(forM_, when)
import System.Process(system)
import System.Exit(ExitCode(..))
import Text.Printf(printf)
import System.IO(hFlush, stdout, hGetEcho, stdin, hSetEcho)
import Control.Exception.Base(bracket_)

-----------------------------------------------------------------------------------------------------------------------

compress  :: [String] -> String -> Maybe String -> IO ()
compress = compress_rar 


compress_rar :: [String] -> String -> Maybe String -> IO ()
compress_rar fileList archiveName mbPassword = do
   let properAchiveName = archiveName ++ ".rar"
   archiveExists <- doesFileExist properAchiveName
   when archiveExists $ error $ printf "Archive \"%s\" allready exists." properAchiveName  
   withListFile fileList $ \fileListName -> do
      properPassword <- getPassword archiveName mbPassword
      case properPassword of
         Nothing -> do
            let cmd = printf "rar a \"%s\" \"@%s\" " properAchiveName fileListName
            system' cmd
            
         Just pass -> do
            let cmd = printf "rar a \"%s\" \"-hp%s\" \"@%s\" " properAchiveName pass fileListName
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


getPassword :: String -> Maybe String -> IO (Maybe String)
getPassword achiveName inPassword = case inPassword of
   Nothing -> return Nothing
   
   Just "" -> do
      printf "Archive %s password: " achiveName
      hFlush stdout
      pass <- withEcho False getLine
      putChar '\n'
      return $ Just pass
 
   Just pass -> return $ Just pass
      
      
withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
              

-----------------------------------------------------------------------------------------------------------------------
