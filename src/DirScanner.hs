
-----------------------------------------------------------------------------------------------------------------------

module DirScanner where

import System.Filesystem(listDir)
import System.Directory(doesFileExist, canonicalizePath)
import Control.Monad(foldM)
import Data.Set(Set)
import qualified Data.Set as Set
import System.FilePath.Glob(match)

import BackupSuite

-----------------------------------------------------------------------------------------------------------------------

type FilePathSet = Set FilePath

class ListFiles lf where
   listFiles :: lf -> FilePathSet -> IO FilePathSet


instance ListFiles FilesSpec where
   listFiles (FileSpec file) fpset = do
      exists <- doesFileExist file
      if exists
         then do 
            path <- canonicalizePath file
            return $ Set.insert path fpset
         else
            return fpset
            
   listFiles (DirSpec dir pattern rec) fpset = do
      files <- listDir dir (match pattern) rec
      return $ fpset `Set.union` Set.fromList files


instance ListFiles l => ListFiles [l] where
   listFiles ls fpset = foldM (flip listFiles) fpset ls
    

instance ListFiles Backup where
   listFiles  (Backup _ enabled includes excludes) fpset = do
   if enabled
      then do      
         filesIncludes <- listFiles includes Set.empty
         filesExcludes <- listFiles excludes Set.empty
         return $ fpset `Set.union` (filesIncludes `Set.difference` filesExcludes)
      else
         return fpset 

-----------------------------------------------------------------------------------------------------------------------
