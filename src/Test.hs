
-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main(main) where

import Test.Framework

import System.Directory
import Data.Yaml

import BackupSuite

-----------------------------------------------------------------------------------------------------------------------

testDir :: FilePath
testDir = "./Test"

main :: IO()
main = do
   setCurrentDirectory testDir
   htfMain htf_thisModulesTests


-----------------------------------------------------------------------------------------------------------------------

test_compileSimpleYaml :: IO ()
test_compileSimpleYaml = do 
   bs  <- decodeFile "Simple.xaml" :: IO (Maybe BackupSuite)
   print bs
   return ()

-----------------------------------------------------------------------------------------------------------------------
