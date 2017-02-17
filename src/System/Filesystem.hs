
module System.Filesystem(listDir) where


import System.Directory
import System.FilePath
import Control.Monad


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
