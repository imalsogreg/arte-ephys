----------------------------------------------------------------------
-- |
-- Module     : Main
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

module Main where

import System.Environment
import Data.Yaml
import DataSource


settingsFilename :: IO String
settingsFilename = fmap (++ settingsName) home
  where home = getEnv "HOME"
        settingsName = "/.arte-ephys/backend.conf"
        

initDataSource :: Object -> Either String DataSource
initDataSource obj = 
  
                  
main :: IO ()
main = do
  
  