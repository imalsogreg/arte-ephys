{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Arte.Common

import qualified System.ZMQ as ZMQ
import qualified Pipes as PP
import Data.Yaml
import System.Directory
import System.Environment
import System.Console.CmdArgs

data ArteMockSpikes = MockCmd
                      { immediateStart    :: Bool
                      , mwlFileExtension  :: String
                      , arteFileExtension :: String
                      , baseDirectory     :: String
                      , searchDepth       :: Int
                      , files             :: [String]
                      } deriving (Show, Data, Typeable)

mockCmd =
  MockCmd { immediateStart = True &= help "Immediate mode"
          , mwlFileExtension = ".tt" &= typ "EXT"
          , arteFileExtension = ".data" &= typ "EXT"
          , baseDirectory = "." &= help "(default \".\")"
          , searchDepth = 0 &= help "Recursion depth for file search"
          , files = [] &= help "List of files to draw spikes from"
          }

main = print =<< cmdArgs mockCmd
{-                     
main :: IO ()
main = do
  (Just netSettings) <- parse =<< readFile (getEnv "HOME")
  let (Just spikeSettings) = lookup netSettings "spkes"
      (myHostname myPortnum) = Vector.head spikeSettings
  fileNames <-
-}