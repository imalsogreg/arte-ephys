module Main where

import Arte.Common.Net
import Arte.Common.NetMessage
import Data.Ephys.EphysDefs
import Data.Ephys.Spike
import Data.Ephys.Cluster
import Data.Ephys.PlaceCell
import Data.Ephys.TrackPosition

import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Either
import qualified System.ZMQ as ZMQ
import Control.Lens
import qualified Data.Serialize as S

getAllSpikeNodes :: Maybe FilePath -> IO [Node]
getAllSpikeNodes configFilePath = 
  forM  ['A'..'Z'] 
  (\l -> getAppNode ("spikes" ++ [l]) configFilePath) >>= \nodes' ->
  return $ rights nodes'

type SpikeHistory = Int -- Placeholder.  Will be more like: KdTree (Vector Voltage) (Field Double)

nullHistory :: SpikeHistory
nullHistory = 0

newPlaceCell :: Track
             -> TVar Trodes
             -> TrodeName
             -> ClusterMethod
             -> IO PlaceCell
newPlaceCell track trodesV t cMethod = do
  trodes <- atomically $ readTVar trodesV
  case Map.lookup t trodes of
    Nothing -> return $ PlaceCell cMethod (Map.fromList [(p,0)|p <- allTrackPos track])
    Just (_, spikeHistoryV) -> do
      return $
        PlaceCell cMethod (Map.fromList [(p,0)|p <- allTrackPos track])  -- TODO: Build from spike history!

-- TODO: So ugly.  Need lens?  Do I have TVars wrapping the wrong things?
setTrodeCluster :: Track
                -> TVar Trodes 
                -> TrodeName
                -> PlaceCellName
                -> ClusterMethod
                -> IO ()
setTrodeCluster track trodesV t c clustMethod = do
  trodes <- atomically $ readTVar trodesV
  newEmptyPlaceCell <- newPlaceCell track trodesV t clustMethod :: IO PlaceCell
  newEmptyTrode'  <- atomically $ do
    sHistory'     <- newTVar nullHistory
    placeCells'   <- newTVar $ Map.fromList [(c,newEmptyPlaceCell)] 
    return (placeCells', sHistory')
  case Map.lookup t trodes of
    Nothing -> let newTrodes = Map.insert t newEmptyTrode' trodes in
      atomically $ writeTVar trodesV newTrodes
    Just (placeCellsMapV,historyV) -> atomically $ do
      placeCellsMap <- readTVar placeCellsMapV
      writeTVar placeCellsMapV (Map.insert c newEmptyPlaceCell placeCellsMap) -- TODO: don't insert empty place
                                                                              -- cell - insert one built from
                                                                              -- the spike history
      writeTVar trodesV (Map.insert t (placeCellsMapV,historyV) trodes)

type Trode = (TVar (Map.Map Int PlaceCell), TVar SpikeHistory)

type Trodes = Map.Map TrodeName Trode

doRequests :: TQueue ArteMessage -> TVar Trodes -> Track -> IO ()
doRequests queue trodesV track = loop
  where loop = do
          (ArteMessage t nFrom nTo mBody) <- atomically $ readTQueue queue
          case mBody of
            Request (TrodeSetCluster tName cName cMethod) ->
              setTrodeCluster track trodesV tName cName cMethod
            Request (TrodeSetAllClusters tName clusts) ->
              mapM_ (\(cName, cMethod) -> setTrodeCluster track trodesV tName cName cMethod)
              (Map.toList clusts)
            Request  r ->
              putStrLn $ unwords ["Caught and ignored request:" ,(take 20 . show $ r),"..."]
            Response r -> 
              putStrLn $ unwords ["Caught and ignored response:",(take 20 . show $ r),"..."]
          case mBody of
            Request ForceQuit -> return ()
            _                 -> loop

main :: IO ()
main = do
  masterNode' <- getAppNode "master" Nothing
  spikeNodes  <- getAllSpikeNodes    Nothing
  incomingSpikes <- atomically $ newTQueue
  case masterNode' of
    Left e -> putStrLn $ "Faulty config file.  Error:" ++ e
    Right masterNode -> withMaster masterNode $ \(fromMaster,toMaster) -> do
      subAs <- forM spikeNodes (\sNode -> async $ enqueueSpikes sNode incomingSpikes)
      mapM wait subAs
      print "Ok"



enqueueSpikes :: Node -> TQueue TrodeSpike -> IO ()
enqueueSpikes spikeNode queue = ZMQ.withContext 1 $ \ctx ->
  ZMQ.withSocket ctx ZMQ.Sub $ \sub -> do
    ZMQ.connect sub $ zmqStr Tcp (spikeNode^.host.ip) (show $ spikeNode^.port)
    ZMQ.subscribe sub ""
    forever $ do
      bs <- ZMQ.receive sub []
      case S.decode bs of
        Right spike -> print "Enqueue" >> (atomically $ writeTQueue queue spike)
        Left  e     -> putStrLn (unwords ["Got a bad value on spike chan.",e])
