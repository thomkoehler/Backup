-----------------------------------------------------------------------------------------------------------------------

module DirScanner(list) where

import System.Filesystem
import System.Directory(doesFileExist, doesDirectoryExist, canonicalizePath)
import Control.Monad(foldM)
import Data.Set(Set)
import qualified Data.Set as Set
import System.FilePath.Glob(match)
import System.FilePath.Lens(filename)
import Control.Lens

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

   listFiles (DirSpec dir pattern recursive) fpset = do
      dirExists <- doesDirectoryExist dir
      if dirExists
         then do
            files <- listDir dir matchFilepath recursive
            canonicalizedFiles <- mapM canonicalizePath files
            return $ fpset `Set.union` Set.fromList canonicalizedFiles
         else
            return fpset
      where
         matchFilepath :: String -> Bool
         matchFilepath path = match pattern $ path ^. filename


instance ListFiles l => ListFiles [l] where
   listFiles ls fpset = foldM (flip listFiles) fpset ls


instance ListFiles Backup where
   listFiles  (Backup _ enabled _ includes excludes) fpset =
      if enabled
         then do
            filesIncludes <- listFiles includes Set.empty
            filesExcludes <- listFiles excludes Set.empty
            return $ fpset `Set.union` (filesIncludes `Set.difference` filesExcludes)
         else
            return fpset


list :: Backup -> IO [FilePath]
list backup = do
   res <- listFiles backup Set.empty
   return $ Set.toList res


-----------------------------------------------------------------------------------------------------------------------
