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

module DataSource where

--import Data.Vector hiding ((++))
--import Data.Text
import Data.Yaml
--import Data.HashMap.Strict (lookup)
--import DataSourceSettings
--import DaqSettings
import Prelude hiding (lookup)


data DataSource = DataSource Int

        
loadSettingsObject :: String -> IO (Either String Object)
loadSettingsObject fn = do 
  settings <- decodeFile fn
  case settings of
    Nothing          -> return $ Left ("File load error on " ++ fn)
    Just settingsObj -> return $ Right settingsObj


{-
loadSettings :: String -> Maybe DataSource
loadSettings settingsFile = 
  do settings <- decodeFile settingsFile
     inFile  <- (lookup "inFile"  settingsJSON)
     outFile <- (lookup "outFile" settingsJSON)
     return $ DataSource 1
-}

    