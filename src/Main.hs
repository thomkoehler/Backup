
-----------------------------------------------------------------------------------------------------------------------

module Main where

import System.Environment(getArgs)

import Options
import BackupSuite

-----------------------------------------------------------------------------------------------------------------------

main :: IO()
main = do
   argv <- getArgs
   opts <- getOptions argv
   suites <- decodeFile $ optInput opts
   print suites
   return ()

-----------------------------------------------------------------------------------------------------------------------