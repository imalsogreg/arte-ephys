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
  case drawOpt of 
    (DrawOccupancy) -> return $ drawField occ
    (DrawDecoding)  -> return $ drawField dPos
    (DrawPlaceCell dUnit') -> do
      dUnit <- readTVarIO dUnit'
      return . drawField $ placeField (dUnit^.dpCell) occ 
    (DrawClusterless tName) ->
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
              white 30 ds draw glossInputs (stepIO track fromMaster)
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

stepIO :: Track -> TQueue ArteMessage -> Float -> DecoderState -> IO DecoderState
stepIO track queue t ds = handleRequests queue ds track

-- handleRequests must be called by stepIO so gloss can treat it
-- as a state.  Otherwise local changes to decoder state won't
-- be seen by the rest of the program.
handleRequests :: TQueue ArteMessage -> DecoderState -> Track -> IO DecoderState
handleRequests queue ds track = loop ds
  where loop ds = do
          let trodesV = ds^.trodes
          (ArteMessage t nFrom nTo mBody) <- atomically $
                                             readTQueue queue
          ds' <- case mBody of
            Request (TrodeSetCluster tName cName cMethod) ->
              setTrodeCluster track ds tName cName cMethod
            Request (TrodeSetAllClusters tName clusts) ->
              foldM (\dState (cName, cMethod) ->
                      setTrodeCluster track dState tName cName cMethod)
              ds (Map.toList clusts)
            Request  r ->
              putStrLn (unwords ["Caught and ignored request:" ,(take 20 . show $ r),"..."]) >>
              return ds
            Response r -> 
              putStrLn (unwords ["Caught and ignored response:",(take 20 . show $ r),"..."]) >>
              return ds 
          case mBody of
            Request ForceQuit -> return ds'
            _                 -> loop ds'

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

-- TODO: This is a place where having TVar in the middle is awkward.
-- I think w/out tvars, I could just use lens to update the maps.  Not sure though.
setTrodeCluster :: Track
                -> DecoderState
                -> TrodeName
                -> PlaceCellName
                -> ClusterMethod
                -> IO DecoderState
setTrodeCluster track ds trodeName placeCellName clustMethod =
  case ds^.trodes of
    Clusterless _ -> error "Tried to set cluster in a clusterless context"
    Clustered tMap -> do
      case Map.lookup trodeName tMap of
        -- if trode doesn't exist, make a new one.  there's no history, so make an empty history
        -- and build a new place cell from that empty history
        Nothing      -> do
          dpc' <- newTVarIO $ DecodablePlaceCell (newPlaceCell track ds trodeName clustMethod) 0
          let newTrode = PlaceCellTrode (Map.fromList [(placeCellName, dpc')]) nullHistory
          return $ (ds & trodes . _Clustered . at trodeName ?~ newTrode)
        Just (PlaceCellTrode pcs sHist) -> do
          case Map.lookup placeCellName pcs of
            Nothing -> do
              dpc' <- newTVarIO $ DecodablePlaceCell (newPlaceCell track ds trodeName clustMethod) 0
              let newTrode = PlaceCellTrode (Map.insert placeCellName dpc' pcs) sHist
              return $ (ds & trodes . _Clustered . at trodeName  ?~ newTrode)
            Just dpc -> do
              atomically . writeTVar dpc $ DecodablePlaceCell (newPlaceCell track ds trodeName clustMethod) 0
              return ds 

newPlaceCell :: Track
             -> DecoderState
             -> TrodeName
             -> ClusterMethod
             -> PlaceCell
newPlaceCell track ds trodeName cMethod = do
  case ds^.trodes of
    Clusterless tMap -> error "Tried to newPlaceCell in clusterless context"
    Clustered   tMap ->
      case Map.lookup trodeName tMap of
        -- No trode by that name, so no history
        Nothing -> PlaceCell cMethod (Map.fromList [(p,0)|p <- allTrackPos track])
        -- Found that trode, build cell to that name from history
        Just (PlaceCellTrode dpc sHist) ->
          PlaceCell cMethod (Map.fromList [(p,0)|p <- allTrackPos track])  -- TODO: Build from spike history! 
