{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}

module Main where

import DecoderState
import DecoderDefs
import DrawingHelpers

import Arte.Common.Net
import Arte.Common.NetMessage
import Data.Ephys.EphysDefs
import Data.Ephys.Spike
import Data.Ephys.Cluster
import Data.Ephys.PlaceCell
import Data.Ephys.Position
import Data.Ephys.TrackPosition
import Data.Ephys.GlossPictures

import Control.Applicative ((<$>),(<*>),pure)
import qualified Data.Traversable as T 
import qualified Data.Map as Map
import Data.Map
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Either
import qualified System.ZMQ as ZMQ
import Control.Lens
import qualified Data.Serialize as S
import qualified Data.Text as Text
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.CircularList as CL
import Control.Monad.State.Strict
import Data.Monoid ((<>))

----------------------------------------
-- TODO: There is way too much STM here
-- UI timing here is handled by gloss,
-- So decoderState top-level fields
-- probably don't need to be in TVars.
-- Also ought to be using lens right
-- here...
---------------------------------------

draw :: DecoderState -> IO Picture
draw ds = do
  pos  <- readTVarIO $ ds^.pos
  occ  <- readTVarIO $ ds^.occupancy
  dPos <- readTVarIO $ ds^. decodedPos
  let trs = ds^.trodes
      trackPicture = drawTrack track
      posPicture = drawPos pos
      drawOpt :: TrodeDrawOption
      drawOpt = case CL.focus `fmap` CL.focus (ds^.trodeDrawOpt) of
        Nothing  -> DrawError "CList error"
        Just (Just opt) -> opt -- weird. I expected fmap to give Just TOpt
  case (ds^.trodes, drawOpt) of
    (_,DrawOccupancy) -> return $ drawField occ
    (_,DrawDecoding)  -> return $ drawField dPos
    (Clustered tMap, DrawPlaceCell tName cName) ->
      case Map.lookup cName <$> Map.lookup tName tMap of 
        Nothing -> return . scale 0.5 0.5 . Text $
                   unwords ["Trode", show tName
                           , " cell", show cName
                           , "not found."]
        Just (Just dUnit') -> do -- weird
          dUnit <- readTVarIO dUnit'
          return . drawField $
            placeField (dUnit^.dpCell) occ 
    (Clusterless tMap, DrawClusterless tName) ->
      return $ scale 0.5 0.5 $ Text "Clusterless Draw not implemented"
    _ -> do
      print "Tried to mix clusterless/clustered decoding/drawing"
      return $ scale 0.5 0.5 $ Text "Mixed clusterless/clustered"

main :: IO ()
main = do
  ds <- initialState
  masterNode' <- getAppNode "master" Nothing
  pNode'      <- getAppNode "pos"    Nothing
  spikeNodes  <- getAllSpikeNodes    Nothing
  incomingSpikes <- atomically $ newTQueue
  case masterNode' of
    Left e -> putStrLn $ "Faulty config file.  Error:" ++ e
    Right masterNode ->
      withMaster masterNode $ \(fromMaster,toMaster) -> do
        subAs <- forM spikeNodes $ \sNode ->
          async $ enqueueSpikes sNode incomingSpikes
        case pNode' of
          Left e -> error $ "No pos node: " ++ e
          Right pNode -> do
            subP <- async $ streamPos pNode ds
            playIO (InWindow "ArteDecoder" (500,400) (10,10))
              white 30 ds draw glossInputs stepIO 
            mapM_ wait subAs
            wait subP 
            print "Past wait subAs"

glossInputs :: Event -> DecoderState -> IO DecoderState
glossInputs e ds =
  case e of
    EventMotion _ -> return ds
    EventKey (SpecialKey k) Up _ _ ->
      return $ ds & trodeDrawOpt %~ (stepDrawOpt k)
    EventKey _ Down _ _ -> return ds
    e -> putStrLn ("Ignoring event " ++ show e) >> return ds

stepIO :: Float -> DecoderState -> IO DecoderState
stepIO _ = return

handleRequests :: TQueue ArteMessage -> DecoderState -> Track -> IO ()
handleRequests queue ds track = loop
  where loop = do
          trodesV <- atomically $ readTVar (ds^.trodes) 
          (ArteMessage t nFrom nTo mBody) <- atomically $
                                             readTQueue queue
          case mBody of
            Request (TrodeSetCluster tName cName cMethod) ->
              setTrodeCluster track ds tName cName cMethod
            Request (TrodeSetAllClusters tName clusts) ->
              mapM_ (\(cName, cMethod) ->
                      setTrodeCluster track ds tName cName cMethod)
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

fanoutSpikeToCells :: DecoderState -> TrodeName -> PlaceCellTrode ->
                      Field Double -> TrodeSpike -> IO ()
fanoutSpikeToCells ds trodeName trode pos spike = do
  _ <- T.mapM (\dpc' -> atomically $ do 
             DecodablePlaceCell pc tauN <- readTVar dpc'
             let f = if spikeInCluster (pc^.cluster) spike then (+1) else (+0)
             writeTVar dpc' $ DecodablePlaceCell
               (stepField (pc) pos spike)
               (f tauN))
       (trode^.dUnits)
  return ()
  
fanoutSpikesToTrodes :: DecoderState -> TQueue TrodeSpike -> IO DecoderState
fanoutSpikesToTrodes ds sQueue = forever $ do
    s <- atomically $ readTQueue sQueue
    let sName = read . Text.unpack . spikeTrodeName $ s
    -- TODO TrodeName is Int, but in TrodeSpike it's Text ..
--    let trodeLookup = \aSpike aMap -> Map.lookup (read. Text.unpack . spikeTrodeName $ s) aMap 
    case ds^.trodes of
      Clustered tMap -> case Map.lookup sName tMap of
        Nothing    -> print "Orphan spike" >> return ds
        Just trode -> do
          p <- readTVarIO (ds^.trackPos)
          ds' <- fanoutSpikeToCells ds sName trode p s
          return $ ds & trodes . _Clustered . ix sName . pcTrodeHistory %~ (<> 1)
      Clusterless tMap -> error "unimplemented" -- TODO

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

-- TODO: see if we can get this out of IO
newPlaceCell :: Track
             -> DecoderState
             -> TrodeName
             -> ClusterMethod
             -> IO PlaceCell
newPlaceCell track ds t cMethod = do
  case ds^.trodes of
    Clusterless tMap -> error "Tried to newPlaceCell in clusterless context"
    Clustered   tMap ->
      -- TODO: Why do we do different things between non-existing and existing?
      --       In both cases, shouldn't we overwrite the old place cell with a
      --       new one, consulting the TRODE's spike history?  I think so.
      case Map.lookup t tMap of
        -- No place cell by that name.
        Nothing -> return $ PlaceCell cMethod (Map.fromList [(p,0)|p <- allTrackPos track])
        -- Found a place cell, replace 
        Just (PlaceCellTrode dpc sHist) -> do
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
