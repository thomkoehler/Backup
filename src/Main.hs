
-----------------------------------------------------------------------------------------------------------------------

module Main where


import Data.Yaml(decodeFileEither, ParseException)

import BackupSuite
import DirScanner

-----------------------------------------------------------------------------------------------------------------------

main::IO()
main = do
   res <- decodeFileEither "Test\\Simple.xaml" :: IO (Either ParseException BackupSuite)
   case res of
      Right bs -> do
         ps <- list bs
         print ps 
      
      Left ex -> error $ show ex 

-----------------------------------------------------------------------------------------------------------------------