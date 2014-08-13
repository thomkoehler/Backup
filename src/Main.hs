
-----------------------------------------------------------------------------------------------------------------------

module Main where


import Data.Yaml(decodeFileEither, ParseException)

import BackupSuite

-----------------------------------------------------------------------------------------------------------------------

main::IO()
main = do
   res <- decodeFileEither "Test\\Simple.xaml" :: IO (Either ParseException BackupSuite)
   case res of
      Left bs -> print $ bs
      Right ex -> error $ show ex 

-----------------------------------------------------------------------------------------------------------------------