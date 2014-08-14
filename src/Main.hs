
-----------------------------------------------------------------------------------------------------------------------

module Main where


import System.FilePath.Glob(compile)

import BackupSuite
import Compressor
import DirScanner

-----------------------------------------------------------------------------------------------------------------------

main::IO()
main = do
   fileList <- list dirBackup
   print fileList
   compress fileList "Test7z" 


dirBackup = Backup
   {
      _bName = "Dir",
      _bEnabled = True,
      _bIncludeFilespecs =
      [
         DirSpec "Test" (compile "*.*") True
      ],
      _bExcludeFilespecs = 
      [
         DirSpec "Test/.HTF" (compile "*.*") True
      ]
   }


-----------------------------------------------------------------------------------------------------------------------