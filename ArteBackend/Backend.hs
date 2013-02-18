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

import Control.Monad
import System.Environment
import Data.Yaml
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
    Nothing          -> return $ Left ("File Error for " ++ fn)
    Just settingsObj -> return $ Right settingsObj

extractSettings :: Object -> Maybe DataSourceSettings
extractSettings _ = undefined

initDataSource :: Object -> IO (Either String DataSource)
initDataSource obj = return $ Left "test"
  
                  
main :: IO ()
main = do
  fn <- settingsFilename
  (Right settings) <- loadSettingsObject fn
  putStrLn $ show settings
  {-
return $ case settings of
             Right obj -> initDataSource obj
                          return ()
             Left  e   -> putStrLn e
  -}
  