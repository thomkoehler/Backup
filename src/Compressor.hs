
-----------------------------------------------------------------------------------------------------------------------

module Compressor(compress) where

import System.IO.Temp(withSystemTempFile)
import System.IO(hPutStrLn, hClose)
import Control.Monad(forM_)
import System.Process(system)
import System.Exit(ExitCode(..))
import Text.Printf(printf)

-----------------------------------------------------------------------------------------------------------------------

withListFile :: [String] -> (FilePath -> IO a) -> IO a
withListFile fileList listFun = 
   withSystemTempFile "Backup" $ \filePath handle -> do
      forM_ fileList (hPutStrLn handle)
      hClose handle
      listFun filePath


compress  :: [String] -> String -> IO ()
compress = compress_7z 

  
compress_7z :: [String] -> String -> IO ()
compress_7z fileList fileName =
   withListFile fileList $ \fileListName -> do
      let cmd = printf "7z.exe a  -r \"-i@%s\" %s" fileListName fileName
      print cmd
      res <- system cmd
      case res of
         ExitSuccess   -> return ()
         ExitFailure _ -> error $ printf "Command %s failed." cmd     
   
-----------------------------------------------------------------------------------------------------------------------
