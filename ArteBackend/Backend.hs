----------------------------------------------------------------------
-- |
-- Module     : Main
-- Copyright  : (c) Greg Hale 2013
-- License    : GPL-3
-- 
-- Maintainer : imalsogreg@gmail.com
-- Stability  : unstable
-- Portability: not portable, uses posix & linux kernel modules
--
-- ArteBackend, a headless interface to nidaq cards for arte-ephys
--
----------------------------------------------------------------------

module Main where

import Control.Monad
import System.Environment
import Data.Yaml
import DaqSettings
import DataSource
import DataSourceSettings


settingsFilename :: IO String
settingsFilename = fmap (++ settingsName) home
  where home = getEnv "HOME"
        settingsName = "/.arte-ephys/backend.conf"
        
loadSettingsObject :: String -> IO (Either String Object)
loadSettingsObject fn = do 
  settings <- decodeFile fn
  case settings of
    Nothing          -> return $ Left ("File load error on " ++ fn)
    Just settingsObj -> return $ Right settingsObj
                  
main :: IO ()
main = do
  fn <- settingsFilename
  (Right settingsObj) <- loadSettingsObject fn
  let dsSettings = loadDaqSettings settingsObj
  case dsSettings of
    Right s ->  putStrLn $ show $ (nChans . head) s
    Left  p ->  putStrLn $ "Problem " ++ p
  