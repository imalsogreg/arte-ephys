{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Ephys.Spike
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.Parse
import Arte.Common

import Pipes.RealTime

import qualified Data.ByteString.Lazy as BSL
import qualified System.ZMQ as ZMQ
import Pipes ( (>->), lift )
import qualified Pipes as P
import Data.Yaml
import System.Directory
import System.Console.CmdArgs
import System.FilePath ((</>))
import Control.Monad.Trans.Writer.Strict
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

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

publishFromQueue :: TQueue TrodeSpike -> ZMQ.Socket ZMQ.Pub -> IO ()
publishFromQueue q sock = do
  s <- atomically readTQueue q
  ZMQ.send sock (encode s) []

pushFileSpikesToQueue :: FilePath -> TQueue TrodeSpike -> IO ()
pushFileSpikesToQueue fp q = do
  f <- BSL.readFile fp
  fi <- getFileInfo fp
  P.runEffect $ produceMWLSpikes' fi f >->
    relativeTimeCat >->
    (P.lift . atomically $ writeTQueue)

main :: IO ()
main = do
  
  opts <- cmdArgs mockCmd
  
  Right myNode <- getAppNode "spikesA" Nothing
    
  let mwlExt = mwlFileExtension opts
      arteExt = arteFileExtension opts          
      sDepth = searchDepth opts
      baseDir = baseDirectory opts
  mwlFileNames  <- getFilesByExtension baseDir sDepth mwlExt
  arteFileNames <- getFilesByExtension baseDir sDepth arteExt
  
  spikeQueue <- newTQueueIO

