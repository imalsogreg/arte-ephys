{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Ephys
import Control.Exception
import Options.Applicative

import Types
import System.Arte.MockData.StreamTTFile
import System.Arte.MockData.StreamPFile


main :: IO ()
main = (execParser opts >>= runStreamer) `catch` (\(e::SomeException) -> print e)
  where
    opts = info (helper <*> dataSourceOpts) (fullDesc
           <> progDesc "Stream data from an mwl or arte file to UDP packtes"
           <> header   "arte-mock-data - a test data source for arte-ephys programs"
           )

runStreamer :: DataSourceOpts -> IO ()
runStreamer opts = case extension $ fileName opts of
  "p" -> streamP opts
  "tt" -> streamTT opts

extension :: String -> String
extension = reverse . takeWhile ((/=) '.') . reverse
