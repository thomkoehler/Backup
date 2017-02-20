-----------------------------------------------------------------------------------------------------------------------

module Options(getOptions, Options(..), CompressType(..), printUsage) where

import System.Console.GetOpt
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
      Option "i" ["input"] (ReqArg (\f opts -> opts { optInput = f }) "input") "XAML backup definition file name (default Backup.xaml)",
      Option "h" ["help"] (NoArg (\opts -> opts { optHelp = True })) "show usage",
      Option "v" ["version"] (NoArg (\opts -> opts { optShowVersion = True })) "show version info",
      Option
         "c"
         ["compress"]
         (ReqArg (\f opts -> opts { optCompressType = read f }) "compress")
         "compress type (Rar|Internal, default Rar)",
      Option
         "b"
         ["backup"]
         (ReqArg (\f opts -> opts { optBackupName = Just f }) "backup")
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
