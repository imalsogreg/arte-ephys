{-# LANGUAGE TemplateHaskell #-}

module Main where

import Arte.Common.Net
import Arte.Common.NetMessage
import Data.Ephys.EphysDefs
import Data.Ephys.Spike
import Data.Ephys.Cluster
import Data.Ephys.PlaceCell
import Data.Ephys.Position
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
import qualified Data.Text as Text



-- Placeholder.  Will be more like: KdTree (Vector Voltage) (Field Double)
type SpikeHistory = Int 

nullHistory :: SpikeHistory
nullHistory = 0



type Trode = (TVar (Map.Map Int PlaceCell), TVar SpikeHistory)

data DecoderState = DecoderState { _pos          :: TVar Position
                                 , _trackPos     :: TVar (Field Double)
                                 , _occupancy    :: TVar (Field Double)
                                 , _lastEstimate :: TVar (Field Double)
                                 , _trodes       :: TVar (Map.Map TrodeName Trode)
                                 }

$(makeLenses ''DecoderState)


main :: IO ()
main = do
  masterNode' <- getAppNode "master" Nothing
  spikeNodes  <- getAllSpikeNodes    Nothing
  incomingSpikes <- atomically $ newTQueue
  case masterNode' of
    Left e -> putStrLn $ "Faulty config file.  Error:" ++ e
    Right masterNode -> withMaster masterNode $ \(fromMaster,toMaster) -> do
      subAs <- forM spikeNodes
               (\sNode -> async $ enqueueSpikes sNode incomingSpikes)
      mapM wait subAs
      print "Ok"

handleRequests :: TQueue ArteMessage -> DecoderState -> Track -> IO ()
handleRequests queue ds track = loop
  where loop = do
          trodesV <- atomically $ readTVar (ds^.trodes) 
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

streamPos :: Node -> DecoderState -> IO ()
streamPos pNode s = ZMQ.withContext 1 $ \ctx ->
  ZMQ.withSocket ctx ZMQ.Sub $ \sub -> do
    ZMQ.connect sub $ zmqStr Tcp (pNode^.host.ip) (show $ pNode^.port)
    ZMQ.subscribe sub ""
    forever $ do
      bs <- ZMQ.receive sub []
      case S.decode bs of
        Left  e -> putStrLn $ "Got a bad Position record." ++ e
        Right p -> do
          atomically $ writeTVar (s^.pos) p
          atomically $ writeTVar (s^.trackPos) (posToField track p kernel)

-- TODO: Make decode general on tracks and kernels.  
track :: Track
track = circularTrack (0,0) 0.57 0.5 0.25 0.15
kernel :: PosKernel
kernel  = PosGaussian 0.2

-- type Trode = (TVar (Map Int PlaceCell), TVar SpikeHistory)

fanoutSpikeToCells :: DecoderState -> Trode -> TrodeSpike -> IO ()
fanoutSpikeToCells ds t s = do
--  clustMap <- readTVarIO . fst $ t
  posF     <- readTVarIO (ds^.trackPos)
  sHistory <- readTVarIO (snd t)
  -- apply stepField to every place cell, b/c this
  -- the stepField function checks if spike is in cluster
  atomically $ modifyTVar (fst t)
    (Map.map (\pc -> stepField pc posF s) )


fanoutSpikesToTrodes :: DecoderState -> TQueue TrodeSpike -> IO ()
fanoutSpikesToTrodes ds sQueue = forever $ do
  (t,s) <- atomically $  do
    ts <- readTVar (ds^.trodes)
    s <- readTQueue sQueue
    -- TODO TrodeName is Int, but in TrodeSpike it's Text ..
    return (Map.lookup (read . Text.unpack . spikeTrodeName $ s) ts, s)
  case (t,s) of
    (Nothing,_) -> return () -- drop the spike
    (Just  t,s) -> do
      atomically $ modifyTVar (snd t) (stepSpikeHistory s)
      fanoutSpikeToCells ds t s

stepSpikeHistory :: TrodeSpike -> SpikeHistory -> SpikeHistory
stepSpikeHistory s sHist = sHist + 1 -- TODO real function

enqueueSpikes :: Node -> TQueue TrodeSpike -> IO ()
enqueueSpikes spikeNode queue = ZMQ.withContext 1 $ \ctx ->
  ZMQ.withSocket ctx ZMQ.Sub $ \sub -> do
    ZMQ.connect sub $
      zmqStr Tcp (spikeNode^.host.ip) (show $ spikeNode^.port)
    ZMQ.subscribe sub ""
    forever $ do
      bs <- ZMQ.receive sub []
      case S.decode bs of
        Right spike ->
          print "Enqueue" >>
          (atomically $ writeTQueue queue spike)
        Left  e     ->
          putStrLn ("Got a bad value on spike chan." ++ e)

getAllSpikeNodes :: Maybe FilePath -> IO [Node]
getAllSpikeNodes configFilePath = 
  forM  ['A'..'Z'] 
  (\l -> getAppNode ("spikes" ++ [l]) configFilePath) >>= \nodes' ->
  return $ rights nodes'

newPlaceCell :: Track
             -> DecoderState
             -> TrodeName
             -> ClusterMethod
             -> IO PlaceCell
newPlaceCell track ds t cMethod = do
  trodes <- atomically $ readTVar (ds^.trodes)
  case Map.lookup t trodes of
    Nothing -> return $ PlaceCell cMethod (Map.fromList [(p,0)|p <- allTrackPos track])
    Just (_, spikeHistoryV) -> do
      return $
        PlaceCell cMethod (Map.fromList [(p,0)|p <- allTrackPos track])  -- TODO: Build from spike history!


-- TODO: So ugly.  Need lens?  Do I have TVars wrapping the wrong things?
-- TODO: We are adding trodes to the world when we get a request for a cluster
--       in a trode that isn't in the Trodes map.  We should probably be getting
--       trodes from a config file shared across computers instead
setTrodeCluster :: Track
                -> DecoderState
                -> TrodeName
                -> PlaceCellName
                -> ClusterMethod
                -> IO ()
setTrodeCluster track ds t c clustMethod = do
  ts <- atomically $ readTVar (ds^.trodes)
  newEmptyPlaceCell <- newPlaceCell track ds t clustMethod :: IO PlaceCell
  newEmptyTrode'  <- atomically $ do
    sHistory'     <- newTVar nullHistory
    placeCells'   <- newTVar $ Map.fromList [(c,newEmptyPlaceCell)] 
    return (placeCells', sHistory')
  case Map.lookup t ts of
    Nothing -> let newTrodes = Map.insert t newEmptyTrode' ts in
      atomically $ writeTVar (ds^.trodes) newTrodes
    Just (placeCellsMapV,historyV) -> atomically $ do
      placeCellsMap <- readTVar placeCellsMapV
      writeTVar placeCellsMapV (Map.insert c newEmptyPlaceCell placeCellsMap) -- TODO: don't insert empty place
                                                                              -- cell - insert one built from
                                                                              -- the spike history
      writeTVar (ds^.trodes) (Map.insert t (placeCellsMapV,historyV) ts)
