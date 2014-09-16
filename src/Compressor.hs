
-----------------------------------------------------------------------------------------------------------------------

module Compressor(compress) where

import System.IO.Temp(withSystemTempFile)
import System.IO(hPutStrLn, hClose, hFlush, stdout, hGetEcho, stdin, hSetEcho)
import System.Directory(doesFileExist)
import Control.Monad(forM_, when)
import System.Process(system)
import System.Exit(ExitCode(..))
import Text.Printf(printf)
import Control.Exception.Base(bracket_)
import Data.Maybe(isJust)

import ZipArchive(addFile)
import Options(Options(..), CompressType(..))

-----------------------------------------------------------------------------------------------------------------------

compress  :: Options -> [String] -> String -> Maybe String -> IO ()
compress options fileList archiveName mbPassword =
   case optCompressType options of
      CtRar -> compressRar fileList archiveName mbPassword
      CtInternal -> compressInternal fileList archiveName mbPassword
        
   
compressInternal :: [String] -> String -> Maybe String -> IO ()
compressInternal fileList archiveName mbPassword = do
   let properAchiveName = archiveName ++ ".zip"
   archiveExists <- doesFileExist properAchiveName
   when archiveExists $ error $ printf "Archive \"%s\" allready exists." properAchiveName
   when (isJust mbPassword) $ error $ printf "Password feature is not supported."
   printf "Creating archive '%s'\n" properAchiveName
   forM_ fileList $ \file -> do
      printf "Adding '%s' ... " file   
      addFile properAchiveName file
      putStrLn "OK" 
      
   putStrLn "Done"
   

compressRar :: [String] -> String -> Maybe String -> IO ()
compressRar fileList archiveName mbPassword = do
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
      printf "Archive %s\n" achiveName
      putStr "Password : "
      hFlush stdout
      pass0 <- withEcho False getLine
      putChar '\n'
      putStr "Confirm password : "
      hFlush stdout
      pass1 <- withEcho False getLine
      putChar '\n'
      if pass0 == pass1
         then return $ Just pass0
         else error "The passwords are not the same."
         
   Just pass -> return $ Just pass
       
      
withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
              

-----------------------------------------------------------------------------------------------------------------------
