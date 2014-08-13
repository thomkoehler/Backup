
-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main(main) where

import Test.Framework

import System.Directory
import Data.Yaml
import System.FilePath.Glob(compile)


import BackupSuite

-----------------------------------------------------------------------------------------------------------------------

testDir :: FilePath
testDir = "./Test"

main :: IO()
main = do
   setCurrentDirectory testDir
   htfMain htf_thisModulesTests


-----------------------------------------------------------------------------------------------------------------------

simpleTestBackupSuite :: BackupSuite
simpleTestBackupSuite = BackupSuite
   {
      _bsName = "SimpleSuite",
      _bsDir = ".",
      _bsBackups = 
      [
         Backup
            {
               _bName = "Backup1",
               _bEnabled = True,
               _bIncludeFilespecs =
               [
                  FileSpec "c:\\Temp\\Test.txt",
                  DirSpec "c:\\Temp" (compile "*.*") True 
               ],
               
               _bExcludeFilespecs =
               [
                  FileSpec "c:\\Temp\\Test1.txt",
                  DirSpec "c:\\Temp" (compile "*.cpp") True 
               ]
            }
      ]
   }


test_compileSimpleYaml :: IO ()
test_compileSimpleYaml = do 
   bs  <- decodeFile "Simple.xaml"
   assertEqual (Just simpleTestBackupSuite) bs
   return ()

-----------------------------------------------------------------------------------------------------------------------
