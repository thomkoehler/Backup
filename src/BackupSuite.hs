
-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BackupSuite where

import Control.Applicative
import Control.Monad
import System.FilePath.Glob(Pattern, compile)
import Data.Aeson
import Control.Lens.TH

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
   deriving(Show)

makeLenses ''FilesSpec


data Backup = Backup
   {
      _bName :: !String,
      _bEnabled :: !Bool,
      _bIncludeFilespecs :: [FilesSpec],
      _bExcludeFilespecs :: [FilesSpec]
   }

makeLenses ''Backup


data BackupSuite = BackupSuite
   {
      _bsName :: String,
      _bsDir :: !FilePath,
      _bsBackups :: [Backup]
   }

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
            
         "file" -> FileSpec <$> v .: "file"
         
         _      -> error $ "type " ++ filespecType ++ "is unknown."
       
   parseJSON _          = mzero


instance FromJSON Backup where
   parseJSON (Object v) = 
      Backup <$> 
         v .: "name" <*> 
         v .:? "enabled" .!= True <*> 
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
        
         
-----------------------------------------------------------------------------------------------------------------------
