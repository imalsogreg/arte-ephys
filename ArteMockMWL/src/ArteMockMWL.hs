{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction #-}

module Main where

import Data.Ephys.Spike
import Data.Ephys.Cluster
import Data.Ephys.Position
import Data.Ephys.TrackPosition
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.Parse
import Data.Ephys.OldMWL.ParseClusterFile
import Data.Ephys.OldMWL.ParsePFile
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
import Control.Concurrent.STM.TMVar
import Control.Concurrent.Async
--import Control.Concurrent.STM.TQueue
import Control.Monad
import Data.Text (Text,pack,unpack)
import qualified Data.Serialize as S
import Control.Lens

data ArteMockSpikes = MockCmd
                      { immediateStart        :: Bool
                      , startExperimentTime   :: Double
                      , spikeFileExtension    :: String
                      , eegFileExtention      :: String
                      , pFileExtention        :: String
                      , clusterBoundsFileName :: String
                      , arteFileExtension     :: String
                      , baseDirectory         :: String
                      , searchDepth           :: Int
                      } deriving (Show, Data, Typeable)

mockCmd :: ArteMockSpikes
mockCmd =
  MockCmd { immediateStart        = True         &= help "Immediate mode"
          , startExperimentTime   = 0.0          &= typ "Time (seconds) in files to start"
          , spikeFileExtension    = "tt"         &= typ "EXT"
          , eegFileExtention      = "eeg"        &= typ "EXT"
          , pFileExtention        = "p"          &= typ "EXT"
          , arteFileExtension     = ".data"      &= typ "EXT"
          , clusterBoundsFileName = "cbfile-run" &= typ "FILENAME"
          , baseDirectory = "." &= help "(default \".\")"
          , searchDepth = 0 &= help "Recursion depth for file search"
          }

queueToNetwork :: (S.Serialize a) => TQueue a -> Node -> IO ()
queueToNetwork q node = do
  let portStr = zmqStr Tcp "*" (show $node ^. port)
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
      relativeTimeCat spikeTime >->
      pipeToQueue q

-- "path/to/0224.tt" -> "24"
trodeNameFromPath :: String -> Text
trodeNameFromPath = pack . reverse . take 2 . drop 3 . reverse

-- "path/to/0224.tt" -> "path/to/cbfile-run"
cbNameFromTTPath :: ArteMockSpikes -> String -> Text
cbNameFromTTPath arg ttPath = pack . (++ (clusterBoundsFileName arg)) .
                              reverse . dropWhile (/= '/') . reverse $ ttPath

pipeToQueue :: TQueue a -> P.Consumer a IO r
pipeToQueue q = forever $ do
  s <- P.await
  P.lift . atomically $ writeTQueue q s

seekAndWait :: TMVar () -> (a -> Double) -> Double -> P.Pipe a a IO () -> P.Pipe a a IO ()
seekAndWait goSignal toTime target produce = do
  PP.dropWhile ((< target) . toTime)
  lift $ print "Seek and wait ready."
  () <- lift . atomically . readTMVar $ goSignal  -- Block here
  produce

orderClusters :: TQueue ArteMessage -> FilePath -> FilePath -> IO ()
orderClusters queue cFile ttFile = do 
  let trodeName = unpack $ trodeNameFromPath ttFile
  cExists <- doesFileExist cFile
  when cExists $ do
    clusters' <- getClusters cFile ttFile
    case clusters' of
      Left _         -> return ()
      Right clusts -> atomically . writeTQueue queue $
                      (ArteMessage 0 "" Nothing (Request $ SetAllClusters trodeName clusts))

main :: IO ()
main = do
  
  opts <- cmdArgs mockCmd
    
  let spikeExt = spikeFileExtension opts
      eegExt   = eegFileExtention opts
      pExt     = pFileExtention opts
      cbName   = clusterBoundsFileName opts
      arteExt = arteFileExtension opts
      
      sDepth = searchDepth opts
      baseDir = baseDirectory opts
  
  [spikeFiles,eegFiles,pFiles,arteFiles] <- mapM (getFilesByExtension baseDir sDepth)
                                  [spikeExt,eegExt,pExt,arteExt]
  let clustFiles = map (cbNameFromTTPath opts) (spikeFiles) :: [Text]
  
  nodes <- mapM (flip getAppNode Nothing) ["spikesA","lfps","pos","master"]
  case nodes of
    [Right spikeNode, Right eegNode, Right pNode, Right masterNode] -> do

      spikeQ <- newTQueueIO
      eegQ   <- newTQueueIO
      posQ   <- newTQueueIO      
      goSign <- newEmptyTMVarIO 

      withMaster masterNode $ \(toMaster,fromMaster) -> do

        print "About to hondle events"
        forkIO $ handleEvents fromMaster goSign  

        spikeAsyncs <- forM spikeFiles $ \fn -> do
          let tName = trodeNameFromPath fn
              cName = cbNameFromTTPath opts fn
          fi' <- getFileInfo fn
          f  <- BSL.readFile fn
          case fi' of
            Left e -> error $ "Bad fileinfo for file " ++ fn ++ " error: " ++ e
            Right fi -> do
              orderClusters toMaster (unpack cName) fn 
              async . P.runEffect $ (dropResult $ produceTrodeSpikes tName fi f) >->
                seekAndWait goSign spikeTime (startExperimentTime opts)
                (relativeTimeCat (\s -> (spikeTime s - startExperimentTime opts))) >->
                pipeToQueue spikeQ

                {-
--        let track = circularTrack (0,0) 0.57 0.5 0.25 0.15
        let p0 = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0
                 ConfSure sZ sZ (-1/0 :: Double) (Location 0 0 0)
            sZ = take 15 (repeat 0)
            ((pX0,pY0),pixPerM,h) = ((166,140),156.6, 0.5)
        posF <- BSL.readFile $ head pFiles
        posAsync <- async . P.runEffect $ dropResult (produceMWLPos posF) >->
                    runningPosition (pX0,pY0) pixPerM h p0 >->
                    seekAndWait goSign _posTime (startExperimentTime opts)
                    (relativeTimeCat (\p -> (_posTime p - startExperimentTime opts))) >->
                    pipeToQueue posQ
-}


        print "About to wait for spike Asyncs"
        mapM_ wait spikeAsyncs
        print "Done waiting"

    _ -> error $ "Problem loading configuration data."

handleEvents :: TQueue ArteMessage -> TMVar () -> IO ()
handleEvents sub goSign = do
  m <- atomically $ readTQueue sub
  case msgBody m of
    Request StartAcquisition -> putStrLn "Got Go signal!" >> atomically (putTMVar goSign ())
    _ -> putStrLn $ "Got and ignored a message: " ++ show m
       