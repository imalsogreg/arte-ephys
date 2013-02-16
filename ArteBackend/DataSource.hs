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
import Data.HashMap.Strict (lookup)
import DataSourceSettings
import DaqSettings
import Prelude hiding (lookup)


data DataSource = DataSource Int

settingsFilename = "/home/greghale/.arte-ephys/backend.conf"

main :: IO ()
main = do
  (Just settings) <- decodeFile settingsFilename :: IO (Maybe Object)
  do 
    (lookup "dataSource" s) of
    Just (Object r) -> initDataSource r
    Nothing         -> do putStrLn "Nope."
                          return $ DataSource 1
  return ()

initDataSource :: Object -> IO DataSource
initDataSource obj = 
  do
    putStrLn $ show obj
    return $ DataSource 1