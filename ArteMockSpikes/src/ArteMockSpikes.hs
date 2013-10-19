{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Arte.Common

import qualified System.ZMQ as ZMQ
import qualified Pipes as PP
import Data.Yaml
import System.Directory
import System.Environment
import System.Console.CmdArgs
import System.FilePath ((</>))
import Control.Monad.Trans.Writer.Strict

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

--main = print =<< cmdArgs mockCmd


main :: IO ()
main = do
  
  opts <- cmdArgs mockCmd
  
  (Just netSettings) <- parse =<< readFile (getEnv "HOME")
  
  let (Just spikeSettings) = lookup netSettings "spkes"
      (myHostname myPortnum) = Vector.head spikeSettings
      mwlExt = mwlFileExtention opts
      arteExt = arteFileExtention opts          
      sDepth = searchDepth opts
      baseDir = baseDirectory opts
  mwlFileNames  <- FUtils.getFilesByExtention baseDir mwlExt  sDepth
  arteFileNames <- FUtils.getFilesByExtention baseDir arteExt sDepth
  
  spikeQueue <- newTQueueIO
  
  