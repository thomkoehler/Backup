
-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BackupSuite where

import Prelude hiding(readFile)
import Control.Applicative
import Control.Monad
import System.FilePath.Glob(Pattern, compile)
import Data.Aeson
import Control.Lens.TH
import Control.Lens
import qualified Data.Yaml as Y
import Data.ByteString(readFile)
import Data.List(elemIndex)

-----------------------------------------------------------------------------------------------------------------------

data FilesSpec
   = FileSpec
      {
         _fsFile :: !FilePath
      }
   | DirSpec
      {
         _dpDir :: !FilePath,
         _dpFilePattern :: !Pattern,
         _dbRecursiv :: !Bool
      }
   deriving(Show, Eq)

makeLenses ''FilesSpec


data Backup = Backup
   {
      _bName :: !String,
      _bEnabled :: !Bool,
      _bPassword :: Maybe String,
      _bIncludeFilespecs :: [FilesSpec],
      _bExcludeFilespecs :: [FilesSpec]
   }
   deriving(Show, Eq)

makeLenses ''Backup


data BackupSuite = BackupSuite
   {
      _bsName :: String,
      _bsDir :: !FilePath,
      _bsBackups :: [Backup]
   }
   deriving(Show, Eq)

makeLenses ''BackupSuite

-----------------------------------------------------------------------------------------------------------------------

instance FromJSON FilesSpec where
   parseJSON (Object v) = do
      filespecType :: String <- v .: "type"
      case filespecType of
         "dir"  -> do
            n <- v .: "name"
            p <- v .: "pattern"
            r <- v .:? "recursiv" .!= False
            return $ DirSpec n (compile p) r

         "file" -> FileSpec <$> v .: "name"

         _      -> error $ "type " ++ filespecType ++ "is unknown."

   parseJSON _          = mzero


instance FromJSON Backup where
   parseJSON (Object v) =
      Backup <$>
         v .: "name" <*>
         v .:? "enabled" .!= True <*>
         v .:? "password" .!= Nothing <*>
         v .: "include" <*>
         v .:? "exclude" .!= []

   parseJSON _          = mzero


instance FromJSON BackupSuite where
   parseJSON (Object v) =
      BackupSuite <$>
         v .:? "name" .!= "" <*>
         v .:? "dir" .!= "." <*>
         v .: "backups"

   parseJSON _          = mzero


decodeFile :: FilePath -> IO [BackupSuite]
decodeFile file = do
   contents <- readFile file
   case Y.decodeEither contents of
      Left err  -> error err
      Right bss -> return bss


loockupBackups :: Maybe String -> BackupSuite -> [Backup]
loockupBackups maybeBackupName suite =
   case maybeBackupName of
      Nothing -> suite ^. bsBackups

      Just name -> let (suiteName, backupName) = splitName name
         in
            if suiteName == "" || suiteName == suite ^. bsName
               then filter (whereBackupName backupName) $ suite ^. bsBackups
               else []

   where
      whereBackupName name backup = name == backup ^. bName

      splitName name = case elemIndex '.' name of
         Nothing -> ("", name)
         Just idx -> (take idx name, drop idx name)


-----------------------------------------------------------------------------------------------------------------------
