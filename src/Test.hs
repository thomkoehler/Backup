
-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main(main) where

import Test.Framework

import System.Directory
import Data.Yaml

import BackupSuite

-----------------------------------------------------------------------------------------------------------------------

testDir = "./Test"

main :: IO()
main = do
   setCurrentDirectory testDir
   htfMain htf_thisModulesTests


-----------------------------------------------------------------------------------------------------------------------

test_compileSimpleYaml :: IO ()
test_compileSimpleYaml = do 
   bs :: (Maybe BackupSuite) <- decodeFile "Simple.xaml"
   return ()

-----------------------------------------------------------------------------------------------------------------------
