
-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main(main) where

import Test.Framework

import System.Directory
import Data.Yaml
import System.FilePath.Glob(compile)


import BackupSuite
import DirScanner

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

test_simpleFile :: IO ()
test_simpleFile = testBackupFileList simpleFileBackup ["dir0/file0.txt"] 
   where
      simpleFileBackup = Backup
         {
            _bName = "Simple File",
            _bEnabled = True,
            _bIncludeFilespecs =
            [
               FileSpec "dir0/file0.txt"
            ],
            _bExcludeFilespecs = []
         }


test_dir :: IO ()
test_dir = testBackupFileList dirBackup ["dir0/file0.txt", "dir1/file1.txt"]
   where
      dirBackup = Backup
         {
            _bName = "Dir",
            _bEnabled = True,
            _bIncludeFilespecs =
            [
               DirSpec "." (compile "*.*") True
            ],
            _bExcludeFilespecs = []
         }


testBackupFileList :: Backup -> [FilePath] -> IO ()
testBackupFileList backup expectedFileList = do
   fl <- list backup
   expected <- mapM canonicalizePath expectedFileList 
   assertEqual expected fl


-----------------------------------------------------------------------------------------------------------------------
