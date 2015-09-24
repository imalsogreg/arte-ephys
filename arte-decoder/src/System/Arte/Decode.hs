{-|
Module      : System.Arte.Decode
Description : Main loop for decoding a single trode
Copyright   : (c) Greg Hale, 2015
                  Shea Levy, 2015
License     : BSD3
Maintainer  : imalsogreg@gmail.com
Stability   : experimental
Portability : GHC, Linux
-}

{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE BangPatterns              #-}

module System.Arte.Decode where

------------------------------------------------------------------------------
import           Control.Applicative                (pure, (<$>), (<*>))
import           Control.Concurrent                 hiding (Chan)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Data.Bool
import qualified Data.Aeson                         as A
import           Data.Aeson                         (Object(..), (.:))
import qualified Data.ByteString.Char8              as BS
import qualified Data.ByteString.Lazy               as BSL
import qualified Data.Foldable                      as F
import qualified Data.List                          as L
import qualified Data.Map.Strict                    as Map
import           Data.Maybe
import qualified Data.Text                          as Text
import           Data.Time.Clock
import qualified Data.Vector                        as V
import qualified Data.Vector.Unboxed                as U
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Data.Color          as Color
import           Options.Applicative
import           Pipes
import qualified Pipes.Prelude                      as P
import           Pipes.RealTime
import           System.Directory
import           System.Exit                        (exitSuccess)
import           System.FilePath                    ((</>))
import           System.IO
import           System.Mem (performGC)
import           Network.Socket
import qualified Network.Socket.ByteString as BS
import           Data.Serialize
------------------------------------------------------------------------------
import           Data.Ephys.Cluster
import           Data.Ephys.EphysDefs
import           Data.Ephys.GlossPictures
import           Data.Ephys.OldMWL.FileInfo
import           Data.Ephys.OldMWL.Parse
import           Data.Ephys.OldMWL.ParseClusterFile
import           Data.Ephys.OldMWL.ParsePFile
import           Data.Ephys.OldMWL.ParseSpike
import           Data.Ephys.PlaceCell
import           Data.Ephys.Position
import           Data.Ephys.Spike
import           Data.Ephys.TrackPosition
import           Data.Map.KDMap
import           System.Arte.FileUtils
import           System.Arte.Net
------------------------------------------------------------------------------
import           System.Arte.Decode.Algorithm
import           System.Arte.Decode.Config
import           System.Arte.Decode.Graphics
import qualified System.Arte.Decode.Histogram as H
import           System.Arte.Decode.Types


------------------------------------------------------------------------------
-- | Get the view option that is currently selected.
focusCursor :: DecoderState -- ^ The current decoding state
            -> TrodeDrawOption
focusCursor ds = fromMaybe (DrawError "Couldn't index at cursor") $
                 (ds^.trodeDrawOpt) ^? ix (ds^.trodeInd) . ix (ds^.clustInd)


------------------------------------------------------------------------------
-- | Generate a 'Picture' displaying the current state.
draw :: DecoderState -- ^ The current decoder state
     -> IO Picture
draw ds = do

  !p    <- readTVarIO $ ds^.pos
  !occ  <- readTVarIO $ ds^.occupancy
  !dPos <- readTVarIO $ ds^.decodedPos :: IO Field

  eHist <- translate (-200) 100    <$> makeHistogramScreenPic (ds^.encodeProf) 100 50
  dHist <- translate (-200) (-100) <$> makeHistogramScreenPic (ds^.decodeProf) 100 50

  let !trackPicture = trackToScreen $ drawTrack defTrack
      !posPicture   = trackToScreen $ drawPos p
      !drawOpt      = focusCursor ds
      !optsPicture  = optsToScreen $ drawOptionsStatePic ds

  selectionPic <- case drawOpt of
    DrawOccupancy          -> return . trackToScreen $ fieldPic occ
    DrawDecoding           -> return . trackToScreen . fieldPic .
                              decodeColormap $ dPos
    DrawPlaceCell _ dUnit' ->  do
      u <- readTVarIO dUnit'
      return . trackToScreen . fieldPic $ placeField (u^.dpCell) occ
    cl@(DrawClusterless _ _ _)  -> mkClusterlessScreenPic cl ds
    DrawError e -> return . scale 50 50 $ text e

  return $ Pictures [trackPicture, posPicture, optsPicture, selectionPic] --, eHist, dHist]


------------------------------------------------------------------------------
-- | Main action for single-trode decoding
main :: IO ()
main = do
  opts <- execParser decoderOpts
  print opts
  ds   <- initialState opts
  dsT  <- newTVarIO ds
  incomingSpikesChan <- atomically newTQueue

  putStrLn "Start pos async"
  posSock <- socket AF_INET Datagram defaultProtocol
  bind posSock $ SockAddrInet (fromIntegral $ psPort $ posSource opts) iNADDR_ANY
  ds' <- readTVarIO dsT
  print "About to posAsync"
  posAsync <- async $ runEffect $ interpretPos opts posSock >->
              (forever $ do
                  p <- await
                  occ <- liftIO $ atomically $ readTVar (ds'^.occupancy)
                  liftIO $ print (V.take 5 . V.fromList . L.reverse . L.sort .  V.toList $ occ)
                  lift . atomically $ do
                    occ <- readTVar (ds^.occupancy)
                    -- TODO property of the camera: 30 fps
                    let posField = V.map (/30) $ posToField defTrack p kernel
                    writeTVar (ds'^.pos) p
                    writeTVar (ds'^.trackPos)  posField
                    when (p^.speed > runningThresholdSpeed)
                      (writeTVar (ds'^.occupancy) (updateField (+) occ posField))
              )

  putStrLn "Start Spike socket asyncs"
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock $ SockAddrInet 5228 iNADDR_ANY

  a <- case clusterless opts of
    True -> do
      let trodeName = 14
      _ <- addClusterlessTrode dsT trodeName
      async $ runEffect $ udpSocketProducer 9000 sock >->
        (forever $ do
            spike <- await
            ds2  <- lift . atomically $ readTVar dsT
            pos2 <- lift . atomically $ readTVar (ds^.trackPos)
            p    <- lift . atomically $ readTVar (ds^.pos)
            when (clessKeepSpike spike) $
              lift (clusterlessAddSpike ds2 trodeName p pos2 spike)
        )

    False -> do
      let cbFilePath = ttDir opts
          cbFile     = cbFilePath </> "cbfile-run"
          trodeName  = 14  -- TODO Ok to name tetrode 0?
      ttFiles <-  getDirectoryContents (ttDir opts)
      print $ "TTFiles: " ++ show ttFiles
      ttFilePath <- (head .  -- TODO s/head/safeHead
                    filter (`endsIn` ".tt"))
                    <$> map (ttDir opts </>) <$>
                    getDirectoryContents (ttDir opts)
      clusters' <- getClusters cbFile ttFilePath
      case clusters' of
       Right clusters -> do
         setTrodeClusters defTrack dsT trodeName clusters
         ds' <- readTVarIO dsT
         case Map.lookup trodeName (ds' ^. trodes._Clustered) of
          Nothing -> error ("shouldn't happen, couldn't find trode named "
                            ++ show trodeName)
          Just pcTrode ->
            async $ runEffect $ udpSocketProducer 9000 sock >->
            (forever $ do
                spike <- await
                let minWid = spikeWidthThreshold defaultClusterlessOpts
                when (spikeWidth spike >= minWid && clessKeepSpike spike) $ do
                  ds2  <- lift . atomically $ readTVar dsT
                  pos2 <- lift . atomically $ readTVar (ds^.trackPos)
                  p    <- lift . atomically $ readTVar (ds^.pos)
                  lift (fanoutSpikeToCells ds2 pcTrode p pos2 spike)
                return ()
            )

  let asyncsAndTrodes = [ (a, undefined) ]

  reconstructionA <-
    async $ if clusterless opts
            then runClusterlessReconstruction
                 defaultClusterlessOpts   0.020 dsT
            else runClusterReconstruction undefined 0.020 dsT

  fakeMaster <- atomically newTQueue
  let spikeAsyncs = map fst asyncsAndTrodes
  runGloss opts dsT
  putStrLn "Closed filehandles"
  putStrLn "wait for asyncs to finish"
  _ <- wait posAsync
  _ <- mapM wait spikeAsyncs
  _ <- wait reconstructionA
  return ()

------------------------------------------------------------------------------
-- | Predicate to drop spikes with low enough amplitudes (< amplutdeThreshold)
--   and short enough widths (< spikeWidthThreshold) in clusterless decoding.
clessKeepSpike :: TrodeSpike -- ^ The spike to consider dropping
               -> Bool
clessKeepSpike s = amp && wid
  where
    opt = defaultClusterlessOpts
    amp = V.maximum (spikeAmplitudes s) >= amplitudeThreshold opt
    wid = spikeWidth s                  >= spikeWidthThreshold opt

-- | Launch gloss window displaying decoding state.
runGloss :: DecoderArgs -- ^ Decoding parameters
         -> TVar DecoderState -- ^ Decoder state
         -> IO ()
runGloss opts dsT = do
  ds <- initialState opts
  playIO (InWindow "ArteDecoder" (700,700) (10,10))
    white 100 ds draw (glossInputs dsT) (stepIO dsT)

------------------------------------------------------------------------------
-- | Gloss callback to handle input events
glossInputs :: TVar DecoderState -- ^ Shared decoder state
            -> Event -- ^ Gloss event
            -> DecoderState -- ^ Pure decoder state from gloss
            -> IO DecoderState
glossInputs dsT e ds =
  let drawOpt = focusCursor ds in
  case e of
    EventMotion _                       -> return ds
    EventKey (SpecialKey KeyEsc) Up _ _ -> exitSuccess
    EventKey (SpecialKey k)      Up _ _ ->
      let f = case k of
            KeyRight -> (trodeInd %~ succ) . (clustInd .~ 0)
            KeyLeft  -> (trodeInd %~ pred) . (clustInd .~ 0)
            KeyUp    -> clustInd  %~ succ
            KeyDown  -> clustInd  %~ pred
            _        -> id
      in atomically (modifyTVar dsT f) >> return (f ds)  -- which copy is actually used?
    EventKey _ Down _ _ -> return ds
    EventKey (MouseButton LeftButton) Up _ (mouseX,mouseY) ->
      case drawOpt of
        (DrawClusterless tName tTrode
         (ClessDraw (XChan cX) (YChan cY))) ->
          let (treeX, treeY) = screenToSpikes mouseX mouseY
              p'  = fromMaybe ((ClusterlessPoint
                                (U.fromList [0,0,0,0]) 1000 (emptyField defTrack)))
                               (ds^.samplePoint)
              p   = Just ((p' & pAmplitude . ix cX .~ r2 treeX) &
                           pAmplitude . ix cY .~ r2 treeY )
              ds' = ds & samplePoint .~ p
          in atomically (writeTVar dsT ds') >> return ds'
        _ -> putStrLn "Ignoring left click" >> return ds
    EventKey (MouseButton RightButton) Up _ _ ->
      case drawOpt of
        (DrawClusterless tName tTrode (ClessDraw cX cY)) ->
          let ds' = ds & samplePoint .~ Nothing
          in  atomically (writeTVar dsT ds') >> return ds'
        _ -> putStrLn "Ignoring right click" >> return ds
    _ -> putStrLn ("Ignoring event " ++ show e) >> return ds



------------------------------------------------------------------------------
-- | Step the state one iteration by replacing the pure decoder state handled
--   by gloss with the mutable shared decoder state.
stepIO :: TVar DecoderState -- ^ Shared decoder state
       -> Float -- ^ Time since last step
       -> DecoderState -- ^ Decoder state from gloss
       -> IO DecoderState
stepIO dsT _ _ = do
  ds' <- readTVarIO dsT
  return ds'



------------------------------------------------------------------------------
-- | Insert a set of cluster boundaries into the state
setTrodeClusters :: Track -- ^ Track definition
                 -> TVar DecoderState -- ^ Decoder state
                 -> TrodeName -- ^ The name of the trode being clustered
                 -> Map.Map PlaceCellName ClusterMethod
                    -- ^ The cluster bounds by place cell name
                 -> IO ()
setTrodeClusters track dsT trodeName clusts  =
  let foldF d (cName,cMethod) =
        setTrodeCluster track d trodeName cName cMethod in
  atomically $ do
    ds  <- readTVar dsT
    ds' <- F.foldlM foldF ds (Map.toList clusts)
    writeTVar dsT ds'


------------------------------------------------------------------------------
-- | Add a clusterless trode to the state
addClusterlessTrode :: TVar DecoderState -- ^ The decoder state
                    -> TrodeName -- ^ The name of the trode to add
                    -> IO (TVar ClusterlessTrode)
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
-- | Apply the point spread function to a position and insert it into the
--   occupancy map and the current position.
updatePos :: TVar DecoderState -- ^ The shared decoder state
          -> Position -- ^ The position to spread
          -> IO ()
updatePos dsT p = let trackPos' = posToField defTrack p kernel in
  atomically $ do
    ds <- readTVar dsT
    modifyTVar' (ds^.occupancy) (updateField (+) trackPos')
    writeTVar (ds^.pos) p
    writeTVar (ds^.trackPos) trackPos'
{-# INLINE updatePos #-}

------------------------------------------------------------------------------
-- | Increment each cluster's place field based on whether a spike falls in
--   its bounds.
fanoutSpikeToCells :: DecoderState -- ^ The decoding state
                   -> PlaceCellTrode -- ^ The trode of interest
                   -> Position -- ^ The most recently seen position
                   -> Field -- ^ Occupancy map
                   -> TrodeSpike -- ^ The spike of interest
                   -> IO ()
fanoutSpikeToCells ds trode pos trackPos spike =
 do
  flip F.mapM_ (trode^.dUnits) $ \dpcT -> do
    DecodablePlaceCell pc tauN <- readTVarIO dpcT
    when (spikeInCluster (pc^.cluster) spike) $ do
      let pc' = if pos^.speed > runningThresholdSpeed
                then pc & countField %~ updateField (+) trackPos
                else pc
          tauN' = tauN + 1
      atomically $ writeTVar dpcT $ DecodablePlaceCell pc' tauN'
{-# INLINE fanoutSpikeToCells #-}
------------------------------------------------------------------------------
-- | Add a spike in the clusterless case
clusterlessAddSpike :: DecoderState -- ^ The decoding state
                    -> TrodeName -- ^ The trode of interest
                    -> Position -- ^ The most recently seen camera position
                    -> Field -- ^ Most recently seen track position
                    -> TrodeSpike -- ^ The spike to add
                    -> IO ()
clusterlessAddSpike ds tName p tPos spike =
  case Map.lookup tName (ds^.trodes._Clusterless) of
    Nothing -> putStrLn "Orphan spike"
    Just t' -> H.timeAction (ds^.encodeProf) $ do
      atomically $ do
        let forKDE = (p^.speed) >= runningThresholdSpeed
        let spike' = (makeCPoint spike tPos,
                      MostRecentTime $ spikeTime spike,
                      forKDE)
        modifyTVar t' $ \(t::ClusterlessTrode) -> t & dtTauN %~ (spike':)
        return ()
{-# INLINE clusterlessAddSpike #-}

------------------------------------------------------------------------------
-- | Convert a spike and spread position into a point for the clusterless
--   KD tree.
makeCPoint :: TrodeSpike -- ^ The spike
           -> Field -- ^ The position
           -> ClusterlessPoint
makeCPoint spike tPos = ClusterlessPoint {
    _pAmplitude = U.convert . spikeAmplitudes $ spike
  , _pWeight    = 1
  , _pField     = normalize $ gt0 tPos
  }

------------------------------------------------------------------------------
-- | Add a cluster to a given trode's list of clusters, creating the trode
--   first if it does not exist.
setTrodeCluster :: Track -- ^ The track model
                -> DecoderState -- ^ The decoding state
                -> TrodeName -- ^ The trode of interest
                -> PlaceCellName -- ^ The name of the cluster
                -> ClusterMethod -- ^ The bounds of the cluster
                -> STM DecoderState
setTrodeCluster track ds trodeName placeCellName clustMethod =
  case ds^.trodes of
    Clusterless _ -> error "Tried to set cluster in a clusterless context"
    Clustered tMap -> do
      dsNewClusts <- case Map.lookup trodeName tMap of
        -- if trode doesn't exist, make a new one.  there's no history,
        -- so make an empty history
        -- and build a new place cell from that empty history
        Nothing      -> do
          dpc' <- newTVar $
                  DecodablePlaceCell
                  (newPlaceCell track clustMethod) 0
          let newTrode =
                PlaceCellTrode
                (Map.fromList [(placeCellName, dpc')]) nullHistory
          return $ (ds & trodes . _Clustered . at trodeName ?~ newTrode)
        Just (PlaceCellTrode pcs sHist) -> do
          case Map.lookup placeCellName pcs of
            Nothing -> do
              dpc' <- newTVar $ DecodablePlaceCell
                      (newPlaceCell track clustMethod) 0
              let newTrode =
                    PlaceCellTrode (Map.insert placeCellName dpc' pcs) sHist
              return $ (ds & trodes . _Clustered . at trodeName  ?~ newTrode)
            Just dpc -> do
              _ <- swapTVar dpc $
                DecodablePlaceCell
                (newPlaceCell track clustMethod) 0
              return ds
      let drawOpts' = clistTrodes $ dsNewClusts^.trodes
      return $
        dsNewClusts { _trodeDrawOpt = drawOpts' }


------------------------------------------------------------------------------
-- | Create a new cluster from a given set of bounds
newPlaceCell :: Track -- ^ Track model
             -> ClusterMethod -- ^ The cluster bounds
             -> PlaceCell
newPlaceCell track cMethod =
  PlaceCell cMethod
  (V.replicate (V.length $ allTrackPos track) 0)


------------------------------------------------------------------------------
-- | Does a given file path end in a certain string?
endsIn :: FilePath -- ^ The path of interest
       -> String -- ^ The suffix
       -> Bool
endsIn fp str = take (length str) (reverse fp) == reverse str

-- | Create a position producer based on the input format
interpretPos :: DecoderArgs -- ^ Arguments for decoding
             -> Socket -- ^ Socket to listen on
             -> Producer Position IO ()
interpretPos DecoderArgs{..} sock = case psPosFormat posSource of
  OatJSON       -> let posProducerNoHistory = udpJsonProducer 9000 sock
                   in  posProducerNoHistory
                       >-> P.map (transOatPosition . unOatPosition)
                       >-> producePos pos0
  ArteBinary    -> udpSocketProducer 9000 sock

-- | Newtype wrapper around Position for different fromJSON instance
newtype OatPosition = OatPosition { unOatPosition :: Position }

-- Hack! OAT will eventually just do the right thing
-- | Translate OAT position coordinates to arte position coordinates
transOatPosition :: Position -- ^ Position in OAT coordinates
                 -> Position
transOatPosition p = let x' = (_x . _location $ p) / 4000 + 229
                         y' = (_y . _location $ p) / 4000 + 21.5
                         z' = (_z . _location $ p)
                     in  p { _location = Location x' y' z'}

instance A.FromJSON OatPosition where
  parseJSON (A.Object v) = do
    posConf         <- bool ConfUnsure ConfSure <$> v .: "pos_ok"
    [pos_x,pos_y]   <- v .: "pos_xy"
    [vel_x,vel_y]   <- v .: "vel_xy"
    return . OatPosition $ (Position
                            (-200)
                            (Location pos_x pos_y 0)
                            (Angle 0 0 0)
                            (atan2 vel_y vel_x)
                            (-300)
                            posConf
                            []
                            []
                            (-400)
                            (Location 0 0 0))
