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
import Control.Monad
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

draw :: TVar DecoderState -> DecoderState -> IO Picture
draw dsT _ = do
  ds   <- readTVarIO $ dsT
  p    <- readTVarIO $ ds^.pos
  occ  <- readTVarIO $ ds^.occupancy
  dPos <- readTVarIO $ ds^.decodedPos

  let trackPicture = drawTrack track
      posPicture = drawPos p
      drawOpt :: TrodeDrawOption
      drawOpt = case CL.focus `fmap` CL.focus (ds^.trodeDrawOpt) of
        Nothing  -> DrawError "CList error"
        Just Nothing -> DrawError "CList CList error"
        Just (Just opt) -> opt -- weird. I expected fmap to give Just TOpt
--  putStrLn $ unwords ["Focus:", show drawOpt, "of options", show (ds^.trodeDrawOpt)]
  field <- case drawOpt of 
    (DrawOccupancy) -> do
--      print "DrawOccupancy"
--      print  $ Map.elems occ
--      print "DrawOccupancy"
      return $ drawNormalizedField occ
    (DrawDecoding)  -> return $ drawNormalizedField dPos
    (DrawPlaceCell dUnit') -> do
      dUnit <- readTVarIO dUnit'
--      print . unwords . map show $ Map.elems (placeField (dUnit^.dpCell) occ)
--      print . unwords . map show $ ds ^.. trodes . _Clustered . traversed . pcTrodeHistory
--      print "DrawPlaceCell"
--      putStrLn $ unwords ["tauN:", show (dUnit^.dpCellTauN)," field:", show(Map.elems $ dUnit^.dpCell.countField)] 
--      print "DrawPlaceCell"
      return . drawNormalizedField $ placeField (dUnit^.dpCell) occ

    (DrawClusterless tName) ->
      return $ scale 0.5 0.5 $ Text "Clusterless Draw not implemented"
    (DrawError e) -> do
      print $ "Draw was told to print DrawError" ++ e
      return $ scale 50 50 $ Text e
  return . scale 100 100 $ pictures [posPicture, trackPicture, field ]

main :: IO ()
main = do
  ds  <- initialState
  dsT <- newTVarIO ds
  masterNode' <- getAppNode "master" Nothing
  pNode'      <- getAppNode "pos"    Nothing
  spikeNodes  <- getAllSpikeNodes    Nothing
  incomingSpikes <- atomically $ newTQueue
  case masterNode' of
    Left e -> putStrLn $ "Faulty config file.  Error:" ++ e
    Right masterNode ->
      withMaster masterNode $ \(toMaster,fromMaster) -> do
        subAs <- forM spikeNodes $ \sNode ->
          async $ enqueueSpikes sNode incomingSpikes
        case pNode' of
          Left e -> error $ "No pos node: " ++ e
          Right pNode -> do
            subP <- async $ streamPos pNode dsT
            dequeueSpikesA <- async . forever $
                              fanoutSpikesToTrodes dsT incomingSpikes -- funny return type -> IO DecStt 
            playIO (InWindow "ArteDecoder" (300,300) (10,10))
              white 30 ds (draw dsT) (glossInputs dsT) (stepIO track fromMaster dsT)
            mapM_ wait subAs 
            wait subP
            _ <- wait dequeueSpikesA
            print "Past wait subAs"

glossInputs :: TVar DecoderState -> Event -> DecoderState -> IO DecoderState
glossInputs dsT e ds =
  case e of
    EventMotion _ -> return ds
    EventKey (SpecialKey k) Up _ _ ->
      atomically $ modifyTVar dsT (trodeDrawOpt %~ (stepDrawOpt k)) >> return ds
    EventKey _ Down _ _ -> return ds   
    e -> putStrLn ("Ignoring event " ++ show e) >> return ds

stepIO :: Track -> TQueue ArteMessage -> TVar DecoderState -> Float -> DecoderState -> IO DecoderState
stepIO track queue dsT t ds = do
  handleRequests queue dsT track
  return ds

-- handleRequests must be called by stepIO so gloss can treat it
-- as a state.  Otherwise local changes to decoder state won't
-- be seen by the rest of the program.
handleRequests :: TQueue ArteMessage -> TVar DecoderState -> Track -> IO ()
handleRequests queue dsT track = --loop ds
--  where loop ds = do
  do
    msg' <- atomically $ tryReadTQueue queue
    ds  <- atomically $ readTVar dsT
    ds' <- case msg' of
      Nothing -> do
        return ds
      Just (ArteMessage t nFrom nTo mBody) -> do
          let trodesV = ds^.trodes
--          (ArteMessage t nFrom nTo mBody) <- atomically $
  --                                           readTQueue queue
          putStrLn $ "Got message" ++ (take 20 . show $ mBody)
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
          return ds' 
    atomically $ writeTVar dsT ds'
 
streamPos :: Node -> TVar DecoderState -> IO ()
streamPos pNode dsT = ZMQ.withContext 1 $ \ctx ->
  ZMQ.withSocket ctx ZMQ.Sub $ \sub -> do
    ZMQ.connect sub $ zmqStr Tcp (pNode^.host.ip) (show $ pNode^.port)
    ZMQ.subscribe sub ""
    print "Finished subscribing"
    forever $ do
      bs <- ZMQ.receive sub []
      case S.decode bs of
        Left  e -> putStrLn $ "Got a bad Position record." ++ e
        Right p -> do
          atomically $ do 
            ds <- readTVar dsT
            let trackPos' = posToField track p kernel
            writeTVar (ds^.pos) p
            writeTVar (ds^.trackPos)   trackPos'
            modifyTVar (ds^.occupancy) (updateField (+) trackPos')
 
fanoutSpikeToCells :: DecoderState -> TrodeName -> PlaceCellTrode ->
                      Field Double -> TrodeSpike -> IO DecoderState
fanoutSpikeToCells ds trodeName trode pos spike = do
  _ <- T.mapM (\dpc' -> do
                  atomically $ do 
                    DecodablePlaceCell pc tauN <- readTVar dpc'
                    let f = if spikeInCluster (pc^.cluster) spike then (+1) else (+0)
                    writeTVar dpc' $ DecodablePlaceCell
                      (stepField (pc) pos spike)
                      (f tauN)
--                  DecodablePlaceCell pc tauN <- atomically $ readTVar dpc'
--                  putStrLn . show $ spikeInCluster (pc^.cluster) spike 
              )
       (trode^.dUnits)
  return ds 
  
fanoutSpikesToTrodes :: TVar DecoderState -> TQueue TrodeSpike -> IO ()
fanoutSpikesToTrodes dsT sQueue = forever $ do 
    ds <- atomically $ readTVar dsT 
    s <- atomically $ readTQueue sQueue
    let sName = read . Text.unpack . spikeTrodeName $ s
    -- TODO TrodeName is Int, but in TrodeSpike it's Text ..
    case ds^.trodes of
      Clustered tMap -> case Map.lookup sName tMap of
        Nothing    -> return ()  -- print "Orphan spike" >> return ds
        Just trode -> do
          p <- readTVarIO (ds^.trackPos)
          ds' <- fanoutSpikeToCells ds sName trode p s
          atomically $ writeTVar dsT 
            (ds' & trodes . _Clustered . ix sName . pcTrodeHistory %~ (+ 1))
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
--      print $ zmqStr Tcp (spikeNode^.host.ip) (show $ spikeNode^.port)
      bs <- ZMQ.receive sub [] 
      case S.decode bs of
        Right spike ->
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
      dsNewClusts <- case Map.lookup trodeName tMap of
        -- if trode doesn't exist, make a new one.  there's no history, so make an empty history
        -- and build a new place cell from that empty history
        Nothing      -> do
          dpc' <- newTVarIO $
                  DecodablePlaceCell
                  (newPlaceCell track ds trodeName clustMethod) 0
          let newTrode =
                PlaceCellTrode
                (Map.fromList [(placeCellName, dpc')]) nullHistory
          return $ (ds & trodes . _Clustered . at trodeName ?~ newTrode)
        Just (PlaceCellTrode pcs sHist) -> do
          case Map.lookup placeCellName pcs of
            Nothing -> do
              dpc' <- newTVarIO $ DecodablePlaceCell
                      (newPlaceCell track ds trodeName clustMethod) 0
              let newTrode =
                    PlaceCellTrode (Map.insert placeCellName dpc' pcs) sHist 
              return $ (ds & trodes . _Clustered . at trodeName  ?~ newTrode)
            Just dpc -> do
              atomically . writeTVar dpc $
                DecodablePlaceCell
                (newPlaceCell track ds trodeName clustMethod) 0
              return ds
      let drawOpts' = clistTrodes $ dsNewClusts^.trodes
--      putStrLn $ unwords ["drawOpt is now:",show drawOpts']
      return $
        dsNewClusts { _trodeDrawOpt = drawOpts' }
   
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
          PlaceCell cMethod (Map.fromList [(p,0)|p <- allTrackPos track])
          -- TODO: Build from spike history! 
