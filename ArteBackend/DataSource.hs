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

module Main where

import Data.Vector
import Data.Text
import Data.Yaml
import Daq

main :: IO ()
main = do
  Just (Object setup_conf) <- (decodeFile "/home/greghale/.arte-ephys/backend.conf") :: IO (Maybe Value)
  let daqsP = (parseMaybe (.: pack "daqs") setup_conf) :: Maybe Array
  case daqsP of
    Just ds -> putStrLn $ (Prelude.concat ( Prelude.map show $ Data.Vector.toList ds))
    Just ds -> putStrLn $ show $ Data.Vector.head ds
    Nothing -> putStrLn "No good"
