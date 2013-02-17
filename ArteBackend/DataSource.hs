----------------------------------------------------------------------
-- |
-- Module     : DataSource.Main
-- Copyright  : (c) Greg Hale 2013
-- License    : GPL3
-- 
-- Maintainer : imalsogreg@gmail.com
-- Stability  : unstable
-- Portability: not portable, uses posix & linux kernel modules
--
-- ArteBackend, a headless interface to nidaq cards for arte-ephys
--
----------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Vector
import Data.Text
import Data.Yaml
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict (lookup)
import DataSourceSettings
import DaqSettings
import Prelude hiding (lookup)


data DataSource = DataSource Int

settingsFilename = "/home/greghale/.arte-ephys/backend.conf"

main :: IO ()
main = do
  m_settings <- decodeFile settingsFilename :: IO (Maybe Object)
  do
    settings <- loadSettings m_settings
 
--    settings        <- m_settings
--    dataSourceJSON  <- settings
--    initDataSource dataSourceJSON

loadSettings :: String -> Maybe DataSource
loadSettings settingsFile = 
  do settings <- decodeFile settingsFile
     inFile  <- (lookup "inFile"  settingsJSON)
     outFile <- (lookup "outFile" settingsJSON)
     return $ DataSource 1

initDataSource :: Object -> Maybe DataSource
initDataSource obj  =
  do
    return $ DataSource 1