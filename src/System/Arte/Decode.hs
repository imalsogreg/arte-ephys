{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Arte.Decode where

import System.Arte.Decode.DecoderState
import System.Arte.Decode.DecoderDefs
import System.Arte.Decode.DrawingHelpers
import System.Arte.Decode.DecodeAlgo

import System.Arte.Net
import System.Arte.NetMessage
import System.Arte.FileUtils
import Data.Ephys.EphysDefs
import Data.Ephys.Spike
import Data.Ephys.Cluster
import Data.Ephys.PlaceCell
import Data.Ephys.Position
import Data.Ephys.TrackPosition
import Data.Ephys.GlossPictures
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.Parse
import Data.Ephys.OldMWL.ParseSpike
import Data.Ephys.OldMWL.ParsePFile
import Data.Ephys.OldMWL.ParseClusterFile
import Data.Map.KDMap

import Data.Time.Clock
import qualified Data.Vector.Unboxed as U
import Pipes.RealTime
import System.Directory
import System.Console.CmdArgs
import Control.Applicative ((<$>),(<*>),pure)
import qualified Data.Text as Text
import qualified Data.Traversable as T
import qualified Data.Foldable    as F
import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Monad
import Control.Concurrent hiding (Chan)
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import Data.Either
import Control.Lens
import qualified Data.Serialize as S
import System.IO
import Pipes
import qualified Pipes.Prelude as P
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.CircularList as CL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL

----------------------------------------
-- TODO: There is way too much STM here
-- UI timing here is handled by gloss,
-- So decoderState top-level fields
-- probably don't need to be in TVars.
--
-- Also ought to be using lens better
-- 
-- Divide up into modules. No big mess
--
-- Get data from distributed-process
---------------------------------------


------------------------------------------------------------------------------
draw :: TVar DecoderState -> DecoderState -> IO Picture
draw _ ds = do

  p    <- readTVarIO $ ds^.pos
  occ  <- readTVarIO $ ds^.occupancy
  dPos <- readTVarIO $ ds^.decodedPos

  let trackPicture = drawTrack track
      posPicture = drawPos p
      drawOpt :: TrodeDrawOption
      drawOpt = case join $ (CL.focus . CL.rotN (ds^.clustInd)) `fmap`
                     CL.focus (CL.rotN (ds^.trodeInd) (ds^.trodeDrawOpt)) of
        Nothing  -> DrawError "CList error"
        Just opt -> opt
      optsPicture = translate (-1) (-1) . scale 0.1 0.1 .
                    scale 0.2 0.2 $ drawDrawOptionsState ds


  field <- case drawOpt of 
    (DrawOccupancy) -> do
      return $ drawNormalizedField occ
    (DrawDecoding)  -> return $ drawNormalizedField
                       (Map.map (\v -> if v > 0.05 then v - 0.05 else 0) dPos)
    (DrawPlaceCell n dUnit') -> do
      dUnit <- readTVarIO dUnit'
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
  ds   <- initialState opts
  dsT  <- newTVarIO ds
  (logSpikes,logDecoding) <- case (doLogging opts) of
    False -> return (Nothing, Nothing)
    True  -> do
      s <- openFile "spikes.txt" WriteMode
      d <- openFile "decoding.txt" WriteMode
      return (Just s, Just d)
  incomingSpikesChan <- atomically newTQueue
  case mwlBaseDirectory opts of
    {-
        "" -> do
          withMaster masterNode $ \(toMaster,fromMaster) -> do
            case pNode' of
              Left e -> error $ "No pos node: " ++ e
              Right pNode -> do
                subP <- async $ streamPos pNode dsT

                subAs <- forM spikeNodes $ \sNode ->
                  async $ enqueueSpikes sNode incomingSpikesChan
                dequeueSpikesA <- async . forever $
                                  fanoutSpikesToTrodes dsT incomingSpikesChan logSpikes

                runGloss dsT fromMaster
--                playIO (InWindow "ArteDecoder" (300,300) (10,10))
--                  white 30 ds (draw dsT) (glossInputs dsT) (stepIO track fromMaster dsT)
                mapM_ wait subAs
                wait subP
                _ <- wait dequeueSpikesA
                print "Past wait subAs"

                putStrLn "start handle-spikes async"
                handleSpikesAsync <-
                  async $
                  fanoutSpikesToTrodes dsT incomingSpikesChan logSpikes
                _ <- wait subP
                _ <- mapM wait subAs
                wait handleSpikesAsync
-}

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
          asyncsAndTrodes <- forM spikeFiles $ \sf -> do
            let tName = read . Text.unpack $ mwlTrodeNameFromPath sf
            print $ "working on file" ++ sf
            fi' <- getFileInfo sf
            case fi' of
              Left e -> error $ unwords ["Error getting info on file",sf,":",e]

              -- Clusterless case ------------------------------------------
              Right fi | clusterless opts -> do
                trodeTVar <- addClusterlessTrode dsT tName
                f <- BSL.readFile sf
                a <- async $ runEffect $
                  dropResult (produceTrodeSpikes tName fi f) >->
                  relativeTimeCat (\s -> spikeTime s - startExperimentTime opts) >->
                  (forever $ do
                      spike <- await
                      ds2  <- lift . atomically $ readTVar dsT
                      pos2 <- lift . atomically $ readTVar (ds^.trackPos)
                      p    <- lift . atomically $ readTVar (ds^.pos)
                      lift $ clusterlessAddSpike ds2 tName p pos2 spike logSpikes
                      return ()
                  )
                return (a, DrawClusterless trodeTVar)

              -- Clustered case ---------------------------------------------
              Right fi | otherwise -> do
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
                        a <- async $ runEffect $ 
                          dropResult (produceTrodeSpikes tName fi f) >->
                          relativeTimeCat (\s -> (spikeTime s - startExperimentTime opts)) >->
                          (forever $ do
                              spike <- await
                              ds2 <- lift . atomically $ readTVar dsT
                              pos2 <- lift . atomically $ readTVar (ds^.trackPos)
                              p    <- lift . atomically $ readTVar (ds^.pos)
                              lift $ fanoutSpikeToCells ds2 tName pcTrode p pos2 spike logSpikes)
                        t <- newTVarIO pcTrode
                        return (a, undefined :: TrodeDrawOption)

--                           (forever $ do
--                               spike <- await
--                               lift . atomically $ writeTQueue incomingSpikesChan spike)

          reconstructionA <-
            async $ if clusterless opts
                    then runClusterlessReconstruction
                         defaultClusterlessOpts   0.02 dsT logDecoding
                    else runClusterReconstruction 0.02 dsT logDecoding
          
          fakeMaster <- atomically newTQueue
          let spikeAsyncs = map fst asyncsAndTrodes
          runGloss opts dsT fakeMaster
          maybe (return ()) hClose logSpikes 
          maybe (return ()) hClose logDecoding
          putStrLn "Closed filehandles"
          putStrLn "wait for asyncs to finish"
          _ <- wait posAsync
          _ <- mapM wait spikeAsyncs
          _ <- wait reconstructionA
          return ()

runGloss :: DecoderArgs -> TVar DecoderState -> TQueue ArteMessage -> IO ()
runGloss opts dsT fromMaster = do
  ds <- initialState opts
  playIO (InWindow "ArteDecoder" (500,500) (10,10))
    white 30 ds (draw dsT) (glossInputs dsT) (stepIO track fromMaster dsT)

pos0 :: Position
pos0 = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0
       ConfSure sZ sZ (-100 :: Double) (Location 0 0 0)
       where sZ = take 5 (repeat 0)

-- caillou/112812clip2 MWL-to-SIUnits TODO make general
posShortcut :: ((Double,Double),Double,Double)
posShortcut = ((166,140),156.6, 0.5)

glossInputs :: TVar DecoderState -> Event -> DecoderState -> IO DecoderState
glossInputs dsT e ds =
  case e of
    EventMotion _ -> return ds
    EventKey (SpecialKey k) Up _ _ ->
      let f = case k of
            KeyRight -> (trodeInd %~ succ) . (clustInd .~ 0)
            KeyLeft  -> (trodeInd %~ pred) . (clustInd .~ 0)
            KeyUp    -> clustInd  %~ succ
            KeyDown  -> clustInd  %~ pred
            _        -> id
      in atomically (modifyTVar dsT f) >> return (f ds)  -- which copy is actually used?
    EventKey _ Down _ _ -> return ds   
    _ -> putStrLn ("Ignoring event " ++ show e) >> return ds

stepIO :: Track -> TQueue ArteMessage -> TVar DecoderState -> Float -> DecoderState -> IO DecoderState
stepIO track queue dsT t ds = do
  -- handleRequests queue dsT track -- TODO this is distributed-process' job
  ds' <- readTVarIO dsT
  return ds'

{-
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
              print $ "SetAllClusters Finished modifying tvar, got opts: "
                ++ show (ds^.trodeDrawOpt)
            Request  r ->
              putStrLn
              (unwords ["Caught and ignored request:"
                       ,(take 20 . show $ r),"..."]) >>
              return ()
            Response r -> 
              putStrLn (unwords ["Caught and ignored response:"
                                ,(take 20 . show $ r),"..."]) >>
              return ()
-}

setTrodeClusters :: Track -> TVar DecoderState -> TrodeName
                    -> Map.Map PlaceCellName ClusterMethod -> IO ()
setTrodeClusters track dsT trodeName clusts  =
  let foldF d (cName,cMethod) =
        setTrodeCluster track d trodeName cName cMethod in
  atomically $ do
    ds  <- readTVar dsT
    ds' <- F.foldlM foldF ds (Map.toList clusts)
    writeTVar dsT ds'


------------------------------------------------------------------------------
addClusterlessTrode ::
  TVar DecoderState -> TrodeName -> IO (TVar ClusterlessTrode)
addClusterlessTrode dsT tName = do
  clusterlessTrode <- newTVarIO $ ClusterlessTrode KDEmpty []
  atomically . modifyTVar dsT $ \ds' ->
    let trodes' = Clusterless $ Map.insert tName clusterlessTrode
                    (ds'^.trodes._Clusterless)
    in
    ds' {_trodes = trodes'
        ,_trodeDrawOpt = clistTrodes trodes'}
  ds' <- atomically $ readTVar dsT
  putStrLn $ unwords ["length: ", show (Map.size $ ds'^.trodes._Clusterless)]

  putStrLn $ unwords ["Added clusterless trode", show tName] -- "trodes:", show t]
  return $! clusterlessTrode


------------------------------------------------------------------------------
updatePos :: TVar DecoderState -> Position -> IO ()
updatePos dsT p = let trackPos' = posToField track p kernel in
  atomically $ do
    ds <- readTVar dsT
    modifyTVar' (ds^.occupancy) (updateField (+) trackPos')
    writeTVar (ds^.pos) p
    writeTVar (ds^.trackPos) trackPos'
{-# INLINE updatePos #-}

------------------------------------------------------------------------------
fanoutSpikeToCells :: DecoderState -> TrodeName -> PlaceCellTrode ->
                      Position -> Field Double -> TrodeSpike -> Maybe Handle -> IO ()
fanoutSpikeToCells ds trodeName trode pos trackPos spike p = do
  flip F.mapM_ (trode^.dUnits) $ \dpcT -> do
    DecodablePlaceCell pc tauN <- readTVarIO dpcT
    when (spikeInCluster (pc^.cluster) spike) $ do
      let pc' = if pos^.speed > runningThresholdSpeed
                then pc & countField %~ updateField (+) trackPos
                else pc
          tauN' = tauN + 1
      tNow <- getCurrentTime
      atomically $ writeTVar dpcT $ DecodablePlaceCell pc' tauN'
      tNow2 <- getCurrentTime
--      when (doLog && (floor (utctDayTime tNow) `mod` 100 == 0)) $ BS.appendFile "spikes.txt" (toBS spike tNow)
      when (isJust p) $ maybe (return ()) (flip BS.hPutStrLn (toBS spike tNow tNow2)) p
--      when (doLog && (floor (utctDayTime tNow) `mod` 20 == 0)) $ modifyMVar (ds^.logData) (flip BS.append (toBS spike tNow))
{-# INLINE fanoutSpikeToCells #-}

toBS :: TrodeSpike -> UTCTime -> UTCTime -> BS.ByteString
toBS spike tNow tNow2 = BS.concat [(BS.pack . show . spikeTime) spike
                            , ", "
                            , (BS.filter (/= 's') . BS.take 9 . BS.pack . show . utctDayTime) tNow
                            , ", "
                            , (BS.filter (/= 's') . BS.take 9 . BS.pack . show . utctDayTime) tNow2]


------------------------------------------------------------------------------
fanoutSpikesToTrodes :: TVar DecoderState -> TQueue TrodeSpike -> Maybe Handle
                     -> IO ()
fanoutSpikesToTrodes dsT sQueue logSpikesH = forever $ do
    s <- atomically $ readTQueue sQueue

    ds <- readTVarIO dsT

    let sName = spikeTrodeName s :: Int
    -- TODO TrodeName is Int, but in TrodeSpike it's Text ..
    case ds^.trodes of
      Clustered tMap -> case Map.lookup (sName :: Int) tMap of
        Nothing    -> do
          --putStrLn ("Orphan spike: " ++ show sName)
          return ()
        Just trode -> do
          p  <- readTVarIO (ds^.pos)
          tp <- readTVarIO (ds^.trackPos)
          fanoutSpikeToCells ds sName trode p tp s logSpikesH
      Clusterless tMap -> return ()


------------------------------------------------------------------------------
clusterlessAddSpike :: DecoderState -> TrodeName -> Position -> Field Double
                    -> TrodeSpike -> Maybe Handle -> IO ()
clusterlessAddSpike ds tName p tPos spike log =
  case Map.lookup tName (ds^.trodes._Clusterless :: Map.Map TrodeName (TVar ClusterlessTrode)) of
    Nothing -> putStrLn "Orphan spike"
    Just t' -> do
      atomically $ do
        tPos <- readTVar (ds^.trackPos)
        let forKDE = (p^.speed) >= runningThresholdSpeed
        let spike' = (makeCPoint spike tPos,
                      MostRecentTime $ spikeTime spike,
                      forKDE) 
        modifyTVar t' $ \(t::ClusterlessTrode) -> t & dtTauN %~ (spike':)
        return ()
{-# INLINE clusterlessAddSpike #-}

------------------------------------------------------------------------------
makeCPoint :: TrodeSpike -> Field Double -> ClusterlessPoint
makeCPoint spike tPos = ClusterlessPoint {
    pAmplitude = U.convert . spikeAmplitudes $ spike
  , pWeight    = 1
  , pField     = normalize $ gt0 tPos
  }

------------------------------------------------------------------------------
stepSpikeHistory :: TrodeSpike -> SpikeHistory -> SpikeHistory
stepSpikeHistory s sHist = sHist + 1 -- TODO real function


------------------------------------------------------------------------------
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
      return $
        dsNewClusts { _trodeDrawOpt = drawOpts' }


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
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
  -- TODO: safeRead instead




------------------------------------------------------------------------------
initialState :: DecoderArgs -> IO DecoderState
initialState opts =
  let zeroField = Map.fromList [(tp,0.1) | tp <- allTrackPos track]
      p0        = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0
                  ConfSure sZ sZ (-1/0 :: Double) (Location 0 0 0)
      sZ        = take 15 (repeat 0)
      clusts    = if clusterless opts
                  then clistTrodes $ Clusterless Map.empty
                  else clistTrodes $ Clusterless Map.empty
  in DecoderState <$>
    newTVarIO p0
    <*> newTVarIO zeroField
    <*> newTVarIO zeroField
    <*> newTVarIO zeroField
    <*> return (Clustered Map.empty)
    <*> newTVarIO zeroField
    <*> pure clusts
    <*> pure 0
    <*> pure 0
    <*> pure False
