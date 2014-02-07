{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import DecoderState
import DecoderDefs
import DrawingHelpers
import DecodeAlgo

import Arte.Common.Net
import Arte.Common.NetMessage
import Arte.Common.FileUtils
import Data.Ephys.EphysDefs
import Data.Ephys.Spike
import Data.Ephys.Cluster
import Data.Ephys.PlaceCell
import Data.Ephys.Position
import Data.Ephys.TrackPosition
import Data.Ephys.GlossPictures
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.Parse
import Data.Ephys.OldMWL.ParsePFile
import Data.Ephys.OldMWL.ParseClusterFile

import Pipes.RealTime
import System.Console.CmdArgs
import System.Directory
import Control.Applicative ((<$>),(<*>),pure)
import qualified Data.Text as Text
import qualified Data.Traversable as T
import qualified Data.Foldable    as F
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Concurrent hiding (Chan)
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
--import Control.Concurrent.Chan
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import Data.Either
import qualified System.ZMQ as ZMQ
import Control.Lens
import qualified Data.Serialize as S
import Pipes
import qualified Pipes.Prelude as P
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.CircularList as CL
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>))

----------------------------------------
-- TODO: There is way too much STM here
-- UI timing here is handled by gloss,
-- So decoderState top-level fields
-- probably don't need to be in TVars.
-- Also ought to be using lens right
-- here...
---------------------------------------

data DecoderArgs = DecoderArgs {mwlBaseDirectory    :: FilePath
                               ,startExperimentTime :: Double}
                 deriving (Show,Data,Typeable)
decoderArgs :: DecoderArgs
decoderArgs = DecoderArgs { mwlBaseDirectory =
                             "" &= 
                             help "Data directory when not taking network data"
                          , startExperimentTime =
                            0 &=
                            help "Start time when spooling from disk directly"
                          }

draw :: TVar DecoderState -> DecoderState -> IO Picture
draw _ ds = do
  -- Node:: Not deadlocking on this read
  p    <- readTVarIO $ ds^.pos
  occ  <- readTVarIO $ ds^.occupancy
  dPos <- readTVarIO $ ds^.decodedPos
  -- End note above
  let trackPicture = drawTrack track
      posPicture = drawPos p
      drawOpt :: TrodeDrawOption
      drawOpt = case join $ CL.focus `fmap` CL.focus (ds^.trodeDrawOpt) of
        Nothing  -> DrawError "CList error"
        Just opt -> opt
      optsPicture = translate (-1) (-1) . scale 0.1 0.1 $ maybe (Text "Opts Problem")
                    (scale 0.2 0.2 . drawDrawOptionsState (ds^.trodeDrawOpt))
                    (join $ CL.focus <$> CL.focus (ds^.trodeDrawOpt))
--  putStrLn $ unwords ["Focus:", show drawOpt, "of options", show (ds^.trodeDrawOpt)]
  field <- case drawOpt of 
    (DrawOccupancy) -> do
      return $ drawNormalizedField occ
    (DrawDecoding)  -> return $ drawNormalizedField
                       (Map.map (\v -> if v > 0.05 then v - 0.05 else 0) dPos)
    (DrawPlaceCell n dUnit') -> do
      dUnit <- readTVarIO dUnit'

--      print . unwords . map show $ Map.elems (placeField (dUnit^.dpCell) occ)
--      print . unwords . map show $ ds ^.. trodes . _Clustered . traversed . pcTrodeHistory
--      print "DrawPlaceCell"
--      putStrLn $ unwords ["tauN:", show (dUnit^.dpCellTauN)," field:", show(Map.elems $ dUnit^.dpCell.countField)] 
--      print "DrawPlaceCell"
--      putStrLn $ "Drawing place cell: " ++ show n
      return . drawNormalizedField $ placeField (dUnit^.dpCell) occ

    (DrawClusterless tName) ->
      return $ scale 0.5 0.5 $ Text "Clusterless Draw not implemented"
    (DrawError e) -> do
      print $ "Draw was told to print DrawError" ++ e
      return $ scale 50 50 $ Text e
  threadDelay 30000
  return . scale 200 200 $ pictures [posPicture, trackPicture, field, optsPicture ]

main :: IO ()
main = do
  opts <- cmdArgs decoderArgs
  print args
  ds   <- initialState
  dsT  <- newTVarIO ds
  masterNode' <- getAppNode "master" Nothing
  pNode'      <- getAppNode "pos"    Nothing
  spikeNodes  <- getAllSpikeNodes    Nothing
  incomingSpikesChan <- atomically newTQueue
  case masterNode' of
    Left e -> putStrLn $ "Faulty config file.  Error:" ++ e
    Right masterNode ->
      case mwlBaseDirectory opts of
        "" -> do
          withMaster masterNode $ \(toMaster,fromMaster) -> do
            case pNode' of
              Left e -> error $ "No pos node: " ++ e
              Right pNode -> do
                subP <- async $ streamPos pNode dsT

                subAs <- forM spikeNodes $ \sNode ->
                  async $ enqueueSpikes sNode incomingSpikesChan
                dequeueSpikesA <- async . forever $
                                  fanoutSpikesToTrodes dsT incomingSpikesChan

                runGloss dsT fromMaster
--                playIO (InWindow "ArteDecoder" (300,300) (10,10))
--                  white 30 ds (draw dsT) (glossInputs dsT) (stepIO track fromMaster dsT)
                mapM_ wait subAs
                wait subP
                _ <- wait dequeueSpikesA
                print "Past wait subAs"

                putStrLn "start handle-spikes async"
                handleSpikesAsync <- async $ fanoutSpikesToTrodes dsT incomingSpikesChan
                _ <- wait subP
                _ <- mapM wait subAs
                wait handleSpikesAsync


        basePath -> do
            putStrLn "Decoder streaming spikes and position from disk"
            incomingSpikesChan <- atomically newTQueue
              
            [spikeFiles,pFiles] <- mapM (getFilesByExtension basePath 2)
                                              ["tt","p"]
            putStrLn $ "spikeFiles: " ++ show spikeFiles
            putStrLn $ "pFiles: "     ++ show pFiles
            let ((pX0,pY0),pixPerM,h) = posShortcut

            putStrLn "Start pos async"
            posAsync <- do
              f <- BSL.readFile (head pFiles)
              ds' <- readTVarIO dsT
              async . runEffect $
                dropResult (produceMWLPos f) >->
                runningPosition (pX0,pY0) pixPerM h pos0 >->
                relativeTimeCatDelayedBy _posTime (negate $ startExperimentTime opts) >->
                (forever $ do
                    p <- await
                    lift . atomically $ do
                      occ <- readTVar (ds^.occupancy)
                      let posField = posToField track p kernel
                      writeTVar (ds'^.pos) p
                      writeTVar (ds'^.trackPos)  posField
                      when (p^.speed > runningThresholdSpeed)
                       (writeTVar (ds'^.occupancy) (updateField (+) occ posField))
                )
                

            putStrLn "Start Spike file asyncs"
            spikeAsyncs <- forM spikeFiles $ \sf -> do
              let tName = read . Text.unpack $ mwlTrodeNameFromPath sf
              print $ "working on file" ++ sf
              fi' <- getFileInfo sf
              case fi' of
                Left e -> error $ unwords ["Error getting info on file",sf,":",e]
                Right fi -> do
                  let cbFilePath = Text.unpack $ cbNameFromTTPath "cbfile-run" sf
                  clusters' <- getClusters cbFilePath sf
                  case clusters' of
                    Left e -> error $ unwords ["Error in clusters from file",cbFilePath,":",e]
                    Right clusters -> do
                      setTrodeClusters track dsT tName clusters
                      ds' <- readTVarIO dsT
                      case Map.lookup tName (ds'^.trodes._Clustered) of
                        Nothing -> error $ unwords ["Shouldn't happen, couldn't find",tName]
                        Just pcTrode -> do
                          f <- BSL.readFile sf

                          async $ runEffect $ 
                            dropResult (produceTrodeSpikes tName fi f) >->
                            relativeTimeCat (\s -> (spikeTime s - startExperimentTime opts)) >->
                            (forever $ do
                                ds2 <- lift . atomically $ readTVar dsT
                                pos2 <- lift . atomically $ readTVar (ds^.trackPos)
                                p    <- lift . atomically $ readTVar (ds^.pos)  -- TODO - await spike should come first!
                                spike <- await
                                lift $ fanoutSpikeToCells ds2 tName pcTrode p pos2 spike)
  
--                           (forever $ do
--                               spike <- await
--                               lift . atomically $ writeTQueue incomingSpikesChan spike)

            reconstructionA <- async $ stepReconstruction 0.01 dsT

            fakeMaster <- atomically newTQueue
            runGloss dsT fakeMaster
            
            putStrLn "wait for asyncs to finish"
            _ <- wait posAsync
            _ <- mapM wait spikeAsyncs
            _ <- wait reconstructionA
            return ()

runGloss :: TVar DecoderState -> TQueue ArteMessage -> IO ()
runGloss dsT fromMaster = do
  ds <- initialState
  playIO (InWindow "ArteDecoder" (500,500) (10,10))
    white 30 ds (draw dsT) (glossInputs dsT) (stepIO track fromMaster dsT)

pos0 :: Position
pos0 = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0
       ConfSure sZ sZ (-100 :: Double) (Location 0 0 0)
       where sZ = take 5 (repeat 0)

posShortcut :: ((Double,Double),Double,Double)
posShortcut = ((166,140),156.6, 0.5)

glossInputs :: TVar DecoderState -> Event -> DecoderState -> IO DecoderState
glossInputs dsT e ds =
  case e of
    EventMotion _ -> return ds
    EventKey (SpecialKey k) Up _ _ -> do
      print "About to do key swap"
      let ds' = ds & trodeDrawOpt %~ stepDrawOpt k
      _ <- atomically $ swapTVar dsT ds'
      print "Done key swap"
      return ds'
    EventKey _ Down _ _ -> return ds   
    e -> putStrLn ("Ignoring event " ++ show e) >> return ds

stepIO :: Track -> TQueue ArteMessage -> TVar DecoderState -> Float -> DecoderState -> IO DecoderState
stepIO track queue dsT t ds = do
  handleRequests queue dsT track
  ds' <- readTVarIO dsT
  return ds'

-- handleRequests must be called by stepIO so gloss can treat it
-- as a state.  Otherwise local changes to decoder state won't
-- be seen by the rest of the program.
handleRequests :: TQueue ArteMessage -> TVar DecoderState -> Track -> IO ()
handleRequests queue dsT track = do
    msg' <- atomically $ tryReadTQueue queue
    case msg' of
      Nothing -> do
        return ()
      Just (ArteMessage t nFrom nTo mBody) -> do
          putStrLn $ "Got message" ++ (take 20 . show $ mBody)
          case mBody of
            Request (TrodeSetCluster tName cName cMethod) -> do
              print "handle about to take"
              atomically $ do
                ds <- readTVar dsT
                setTrodeCluster track ds tName cName cMethod
                return ()
            Request (TrodeSetAllClusters tName clusts) -> do
              setTrodeClusters track dsT tName clusts

{-
              let foldF dState (cName,cMethod) =
                    setTrodeCluster track dState tName cName cMethod
              print "SetAllClusters About to modifyMVar"
              atomically $ do
                ds <- readTVar dsT
                ds' <- F.foldlM foldF ds (Map.toList clusts)
                writeTVar dsT ds'
                return ()
  -}
              
              ds <- readTVarIO dsT
              print $ "SetAllClusters Finished modifying tvar, got opts: " ++ show (ds^.trodeDrawOpt)
            Request  r ->
              putStrLn (unwords ["Caught and ignored request:" ,(take 20 . show $ r),"..."]) >>
              return ()
            Response r -> 
              putStrLn (unwords ["Caught and ignored response:",(take 20 . show $ r),"..."]) >>
              return ()

setTrodeClusters :: Track -> TVar DecoderState -> TrodeName
                    -> Map.Map PlaceCellName ClusterMethod -> IO ()
setTrodeClusters track dsT trodeName clusts  =
  let foldF d (cName,cMethod) =
        setTrodeCluster track d trodeName cName cMethod in
  atomically $ do
    ds  <- readTVar dsT
    ds' <- F.foldlM foldF ds (Map.toList clusts)
    writeTVar dsT ds'

streamPos :: Node -> TVar DecoderState -> IO ()
streamPos pNode dsT = ZMQ.withContext 1 $ \ctx ->
  ZMQ.withSocket ctx ZMQ.Sub $ \sub -> do
    ZMQ.connect sub $ zmqStr Tcp (pNode^.host.ip) (show $ pNode^.port)
    ZMQ.subscribe sub ""
--    print "Finished subscribing"
    forever $ do
      bs <- ZMQ.receive sub []
      case S.decode bs of
        Left  e -> putStrLn $ "Got a bad Position record." ++ e
        Right p -> updatePos dsT p

updatePos :: TVar DecoderState -> Position -> IO ()
updatePos dsT p = let trackPos' = posToField track p kernel :: Map.Map TrackPos Double in
  atomically $ do
    ds <- readTVar dsT
    modifyTVar' (ds^.occupancy) (updateField (+) trackPos')
    writeTVar (ds^.pos) p
    writeTVar (ds^.trackPos) trackPos'

fanoutSpikeToCells :: DecoderState -> TrodeName -> PlaceCellTrode ->
                      Position -> Field Double -> TrodeSpike -> IO ()
fanoutSpikeToCells ds trodeName trode pos trackPos spike = do
  flip F.mapM_ (trode^.dUnits) $ \dpcT -> do
    DecodablePlaceCell pc tauN <- readTVarIO dpcT
    when (spikeInCluster (pc^.cluster) spike) $ do
      let pc' = if pos^.speed > runningThresholdSpeed
                then pc & countField %~ updateField (+) trackPos
                else pc
          tauN' = tauN + 1
      atomically . writeTVar dpcT $ DecodablePlaceCell pc' tauN'
  
fanoutSpikesToTrodes :: TVar DecoderState -> TQueue TrodeSpike -> IO ()
fanoutSpikesToTrodes dsT sQueue = forever $ do
    s <- atomically $ readTQueue sQueue

    ds <- readTVarIO dsT

    let sName = spikeTrodeName s :: Int
    -- TODO TrodeName is Int, but in TrodeSpike it's Text ..
    case ds^.trodes of
      Clustered tMap -> case Map.lookup (sName :: Int) tMap of
        Nothing    -> do
          --putStrLn ("Orphan spike: " ++ show sName)
          --return ds
          return ()
        Just trode -> do
          p  <- readTVarIO (ds^.pos)
          tp <- readTVarIO (ds^.trackPos)
          fanoutSpikeToCells ds sName trode p tp s
--          return $ 
--            (ds' & trodes . _Clustered . ix sName . pcTrodeHistory %~ (+ 1))
      Clusterless tMap -> return ()
--    return ds'


stepSpikeHistory :: TrodeSpike -> SpikeHistory -> SpikeHistory
stepSpikeHistory s sHist = sHist + 1 -- TODO real function

{-
-- Takes the place of enqueue and fanoutToTrodes.  performance experiment
handleSpikesNoQueue :: Node -> TVar DecoderState -> IO () 
handleSpikesNoQueue spikeNode dsT = do
  ZMQ.withContext 1 $ \ctx -> ZMQ.withSocket ctx ZMQ.Sub $ \sub -> do
    ZMQ.connect sub $
      zmqStr Tcp (spikeNode^.host.ip) (show $ spikeNode^.port)
    ZMQ.subscribe sub ""
    forever $ do
      bs <- ZMQ.receive sub []
      case S.decode bs of
        Left e -> putStrLn ("Got a bad value on spike chan: " ++ e)
        Right spike -> do
          let sName = spikeTrodeName spike :: Int
          ds <- readTVarIO dsT
          case ds^.trodes of
            Clustered tMap -> case Map.lookup (sName :: Int) tMap of
              Nothing -> return ()
              Just trode -> do
                p   <- readTVarIO (ds^.trackPos)
                ds' <- fanoutSpikeToCells ds sName trode p spike
                atomically . writeTVar dsT $
                  (ds' & trodes . _Clustered . ix sName . pcTrodeHistory %~ (+1))
-}

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
        Right spike -> do
          atomically $ writeTQueue queue spike
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
                -> STM DecoderState
setTrodeCluster track ds trodeName placeCellName clustMethod =
  case ds^.trodes of
    Clusterless _ -> error "Tried to set cluster in a clusterless context"
    Clustered tMap -> do
--      print $ "Adding placeCell name " ++ show placeCellName 
      dsNewClusts <- case Map.lookup trodeName tMap of
        -- if trode doesn't exist, make a new one.  there's no history, so make an empty history
        -- and build a new place cell from that empty history
        Nothing      -> do
          dpc' <- newTVar $
                  DecodablePlaceCell
                  (newPlaceCell track ds trodeName clustMethod) 0
          let newTrode =
                PlaceCellTrode
                (Map.fromList [(placeCellName, dpc')]) nullHistory
          return $ (ds & trodes . _Clustered . at trodeName ?~ newTrode)
        Just (PlaceCellTrode pcs sHist) -> do
          case Map.lookup placeCellName pcs of
            Nothing -> do
              dpc' <- newTVar $ DecodablePlaceCell 
                      (newPlaceCell track ds trodeName clustMethod) 0
              let newTrode =
                    PlaceCellTrode (Map.insert placeCellName dpc' pcs) sHist 
              return $ (ds & trodes . _Clustered . at trodeName  ?~ newTrode)
            Just dpc -> do
              _ <- swapTVar dpc $ 
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

orderClusters :: TQueue ArteMessage -> FilePath -> FilePath -> IO ()
orderClusters queue cFile ttFile = do 
  let trodeName = Text.unpack $ mwlTrodeNameFromPath ttFile
  cExists <- doesFileExist cFile
  when cExists $ do
    clusters' <- getClusters cFile ttFile
    case clusters' of
      Left _         -> return ()
      Right clusts -> atomically . writeTQueue queue $
                      (ArteMessage 0 "" Nothing (Request $ TrodeSetAllClusters (read trodeName) clusts))
  -- TODO: Unsafe use of read!!
