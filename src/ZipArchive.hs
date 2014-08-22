-----------------------------------------------------------------------------------------------------------------------

module ZipArchive(addFile) where

import System.IO
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as B

import System.Directory(doesFileExist)

-----------------------------------------------------------------------------------------------------------------------

addFile :: FilePath -> FilePath -> IO ()
addFile archiveName file = do
   archiveExists <- doesFileExist archiveName
   if archiveExists
      then do
         withFile archiveName ReadWriteMode $ \handle -> do
            contents <- B.hGetContents handle
            let archive = toArchive contents
            newArchive <- addFilesToArchive [] archive [file]
            B.hPut handle $ fromArchive newArchive
            
      else do
         archive <- addFilesToArchive [] emptyArchive [file]  
         B.writeFile archiveName $ fromArchive archive

-----------------------------------------------------------------------------------------------------------------------
