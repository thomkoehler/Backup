-----------------------------------------------------------------------------------------------------------------------

module Options(getOptions, Options(..), CompressType(..), printUsage) where

import System.Console.GetOpt
import Data.Maybe(fromMaybe)
import Data.List(isPrefixOf)

-----------------------------------------------------------------------------------------------------------------------

data CompressType = CtRar | CtInternal deriving(Show)

instance Read CompressType where
   readsPrec _ = tryParse [("Rar", CtRar), ("Internal", CtInternal)]

data Options = Options
   {
      optInput :: FilePath,
      optBackupName :: Maybe String,
      optCompressType :: CompressType,
      optHelp :: Bool,
      optShowVersion :: Bool
   }
   deriving(Show)


defaultOptions :: Options
defaultOptions = Options
   {
      optBackupName = Nothing,
      optInput = "Backup.xaml",
      optCompressType = CtRar,
      optHelp = False,
      optShowVersion = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [
      Option "i" [] (OptArg ((\f opts -> opts { optInput = f }) . fromMaybe "input") "FILE") "XAML backup definition file name (default Backup.xaml)",
      Option "h" ["help"] (NoArg (\opts -> opts { optHelp = True })) "show usage",
      Option "v" ["version"] (NoArg (\opts -> opts { optShowVersion = True })) "show version info",
      Option
         "c"
         ["compress"]
         (OptArg ((\f opts -> opts { optCompressType = read f }) . fromMaybe "compress") "COMPRESS-TYPE")
         "compress type (Rar|Internal, default Rar)",
      Option
         "b"
         ["backup"]
         (OptArg ((\f opts -> opts { optBackupName = Just f }) . fromMaybe "backup") "SUITE-NAME.BACKUP-NAME")
         "backup selection (default all)"
   ]


usageHeader :: String
usageHeader = "Usage: Backup [OPTION...]"

getOptions :: [String] -> IO Options
getOptions argv =
   case getOpt Permute options argv of
      (o, _, []) -> return $ foldl (flip id) defaultOptions o
      (_ ,_, errs) -> error $ concat errs ++ usageInfo usageHeader options


printUsage :: IO ()
printUsage = putStrLn $ usageInfo usageHeader options


tryParse :: [(String, a)] -> ReadS a
tryParse [] _ = []
tryParse ((token, value) : xs) line =
   if token `isPrefixOf` line
      then [(value, drop (length token) line)]
      else tryParse xs line

-----------------------------------------------------------------------------------------------------------------------
