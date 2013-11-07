{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Ephys.Spike
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.Parse
import Arte.Common

import Pipes.RealTime

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified System.ZMQ as ZMQ
import Pipes ( (>->), lift )
import qualified Pipes as P
import qualified Pipes.Prelude as PP
import Data.Yaml
import System.Directory
import System.Console.CmdArgs
import System.FilePath ((</>))
import Control.Monad.Trans.Writer.Strict
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
--import Control.Concurrent.STM.TQueue
import Control.Monad
import Data.Text (Text,pack)
import qualified Data.Serialize as S
import Control.Lens

data ArteMockSpikes = MockCmd
                      { immediateStart    :: Bool
                      , mwlFileExtension  :: String
                      , arteFileExtension :: String
                      , baseDirectory     :: String
                      , searchDepth       :: Int
                      , files             :: [String]
                      } deriving (Show, Data, Typeable)

mockCmd :: ArteMockSpikes
mockCmd =
  MockCmd { immediateStart = True &= help "Immediate mode"
          , mwlFileExtension = ".tt" &= typ "EXT"
          , arteFileExtension = ".data" &= typ "EXT"
          , baseDirectory = "." &= help "(default \".\")"
          , searchDepth = 0 &= help "Recursion depth for file search"
          , files = [] &= help "List of files to draw spikes from"
          }

queueToNetwork :: TQueue TrodeSpike -> Node -> IO ()
queueToNetwork q node = do
  let port    = node ^. nodePort
      portStr = "tcp://*:" ++ show port
  ZMQ.withContext 1 $ \ctx -> do
    ZMQ.withSocket ctx ZMQ.Pub $ \pubSock -> do
      ZMQ.bind pubSock portStr
      forever $ do
        s <- atomically $ readTQueue q
        ZMQ.send pubSock (S.encode s) []

pushMWLFileSpikesToQueue :: FilePath -> TQueue TrodeSpike -> IO ()
pushMWLFileSpikesToQueue fp q = do
  f <- BSL.readFile fp
  let tName = trodeNameFromPath fp
  eFi <- getFileInfo fp
  case eFi of
    Left e -> putStrLn ("Error with file " ++ fp ++ " : " ++ e)
    Right fi -> 
      P.runEffect $ dropResult (produceMWLSpikes fi f) >->
      PP.map (mwlToArteSpike fi tName) >->
      relativeTimeCat >->
      pipeToQueue q

-- "path/to/0224.tt" -> "24"
trodeNameFromPath :: String -> Text
trodeNameFromPath = pack . reverse . take 2 . drop 3 . reverse

pipeToQueue :: TQueue TrodeSpike -> P.Consumer TrodeSpike IO r
pipeToQueue q = forever $ do
  s <- P.await
  P.lift . atomically $ writeTQueue q s

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
  
  fileHandles <- forM mwlFileNames $ \fn ->
    async $ pushMWLFileSpikesToQueue fn spikeQueue

  pubHandle <- async $ queueToNetwork spikeQueue myNode

  mapM_ wait fileHandles
  _ <- wait pubHandle

  print "Done waiting"