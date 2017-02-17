
module System.Filesystem(listDir) where


import qualified Data.ByteString as B
import System.Directory(getDirectoryContents, doesFileExist, doesDirectoryExist, canonicalizePath, getModificationTime)
import System.FilePath((</>), takeFileName)
import Control.Monad(forM, forM_, filterM, when)
import System.IO(hFileSize, openFile, hClose, IOMode(ReadMode), hIsEOF, hSeek, SeekMode(RelativeSeek))
import Text.Printf(printf)
import Data.List(intersect, (\\))
import Control.Exception.Base(bracket)


listDir :: FilePath -> (FilePath -> Bool) -> Bool -> IO [FilePath]
listDir topdir fileSelect recursive = do
   names <- getDirectoryContents topdir
   let properNames = filter (`notElem` [".", ".."]) names
   paths <- forM properNames $ \name -> do
      let path = topdir </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory && recursive
         then listDir path fileSelect True
         else
            if fileSelect path
               then return [path]
               else return []
   return (concat paths)
