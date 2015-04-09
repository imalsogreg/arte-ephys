module Main where

import Data.Ephys
import Options.Applicative

import Types
import System.Arte.MockData.StreamTTFile
import System.Arte.MockData.StreamPFile


main :: IO ()
main = execParser opts >>= runStreamer
  where
    opts = info (helper <*> dataSourceOpts) (fullDesc
           <> progDesc "Stream data from an mwl or arte file to UDP packtes"
           <> header   "arte-mock-data - a test data source for arte-ephys programs"
           )

runStreamer :: DataSourceOpts -> IO ()
runStreamer = streamTT
