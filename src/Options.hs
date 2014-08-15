-----------------------------------------------------------------------------------------------------------------------

module Options(getOptions, Options(..)) where

import System.Console.GetOpt
import Data.Maybe(fromMaybe)

-----------------------------------------------------------------------------------------------------------------------

data Options = Options
   {
      optInput :: FilePath,
      optBackupName :: Maybe String
   }  
   deriving(Show)
   
   
defaultOptions :: Options
defaultOptions = Options
   {
      optBackupName = Nothing,
      optInput = "Backup.xaml"
   }

options :: [OptDescr (Options -> Options)]
options = 
   [
      Option ['i'] [] (OptArg ((\f opts -> opts { optInput = f }) . fromMaybe "input") "FILE") "input FILE",
      Option 
         ['b'] 
         ["backup"] 
         (OptArg ((\f opts -> opts { optBackupName = Just f }) . fromMaybe "backup") "BACKUP-NAME") 
         "backup BACKUP-NAME"
   ]    

   
getOptions :: [String] -> IO Options
getOptions argv = 
   case getOpt Permute options argv of
      (o, _, []) -> return $ foldl (flip id) defaultOptions o
      (_ ,_, errs) -> error $ (concat errs) ++ usageInfo header options
         where 
            header = "Usage: Backup [OPTION...]"
   

-----------------------------------------------------------------------------------------------------------------------
