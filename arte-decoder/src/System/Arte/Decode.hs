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
import qualified Data.Aeson                         as A
import           Data.Aeson                         (Object(..), (.:))
import qualified Data.ByteString.Char8              as BS
import qualified Data.ByteString.Lazy               as BSL
import qualified Data.Foldable                      as F
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
--import           System.Arte.NetMessage
------------------------------------------------------------------------------
import           System.Arte.Decode.Algorithm
import           System.Arte.Decode.Config
import           System.Arte.Decode.Graphics
import qualified System.Arte.Decode.Histogram as H
import           System.Arte.Decode.Types


------------------------------------------------------------------------------
focusCursor :: DecoderState -> TrodeDrawOption
focusCursor ds = fromMaybe (DrawError "Couldn't index at cursor") $
                 (ds^.trodeDrawOpt) ^? ix (ds^.trodeInd) . ix (ds^.clustInd)

atCursor ds = trodeDrawOpt . ix (ds^.trodeInd) . ix (ds^.clustInd)


------------------------------------------------------------------------------
draw :: TVar DecoderState -> DecoderState -> IO Picture
draw _ ds = do

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
main :: IO ()
main = do
  opts <- execParser decoderOpts
  print opts
  ds   <- initialState opts
  dsT  <- newTVarIO ds
  (logSpikes,logDecoding) <- case (doLogging opts) of
    False -> return (Nothing, Nothing)
    True  -> do
      s <- openFile "spikes.txt" WriteMode
      d <- openFile "decoding.txt" WriteMode
      return (Just s, Just d)
  incomingSpikesChan <- atomically newTQueue

  let ((pX0,pY0),pixPerM,h) = posShortcut

  putStrLn "Start pos async"
  posSock <- socket AF_INET Datagram defaultProtocol
  bind posSock $ SockAddrInet (fromIntegral $ psPort $ posSource opts) iNADDR_ANY
  ds' <- readTVarIO dsT
  print "About to posAsync"
  posAsync <- async $ runEffect $ interpretPos opts posSock >->
              (forever $ do
                  p <- await
                  lift $ print "Forever awaiting"
                  lift . atomically $ do
                    occ <- readTVar (ds^.occupancy)
                    let posField = posToField defTrack p kernel
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
              lift (clusterlessAddSpike ds2 trodeName p pos2 spike logSpikes)
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
                  lift (fanoutSpikeToCells ds2 trodeName pcTrode p pos2 spike logSpikes)
                return ()
            )

  let asyncsAndTrodes = [ (a, undefined) ]

  reconstructionA <-
    async $ if clusterless opts
            then runClusterlessReconstruction
                 defaultClusterlessOpts   0.020 dsT logDecoding
            else runClusterReconstruction undefined 0.020 dsT logDecoding

  fakeMaster <- atomically newTQueue
  let spikeAsyncs = map fst asyncsAndTrodes
  runGloss opts dsT
  maybe (return ()) hClose logSpikes
  maybe (return ()) hClose logDecoding
  putStrLn "Closed filehandles"
  putStrLn "wait for asyncs to finish"
  _ <- wait posAsync
  _ <- mapM wait spikeAsyncs
  _ <- wait reconstructionA
  return ()

------------------------------------------------------------------------------
clessKeepSpike :: TrodeSpike -> Bool
clessKeepSpike s = amp && wid
  where
    opt = defaultClusterlessOpts
    amp = V.maximum (spikeAmplitudes s) >= amplitudeThreshold opt
    wid = spikeWidth s                  >= spikeWidthThreshold opt


runGloss :: DecoderArgs -> TVar DecoderState -> IO ()
runGloss opts dsT = do
  ds <- initialState opts
  playIO (InWindow "ArteDecoder" (700,700) (10,10))
    white 100 ds (draw dsT) (glossInputs dsT) (stepIO defTrack dsT)

pos0 :: Position
pos0 = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0
       ConfSure sZ sZ (-100 :: Double) (Location 0 0 0)
       where sZ = take 5 (repeat 0)

-- caillou/112812clip2 MWL-to-SIUnits TODO make general
posShortcut :: ((Double,Double),Double,Double)
posShortcut = ((166,140),156.6, 0.5)


------------------------------------------------------------------------------
glossInputs :: TVar DecoderState -> Event -> DecoderState -> IO DecoderState
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
stepIO :: Track -> TVar DecoderState ->
          Float -> DecoderState -> IO DecoderState
stepIO track dsT t ds = do
  ds' <- readTVarIO dsT
  return ds'



------------------------------------------------------------------------------
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
updatePos dsT p = let trackPos' = posToField defTrack p kernel in
  atomically $ do
    ds <- readTVar dsT
    modifyTVar' (ds^.occupancy) (updateField (+) trackPos')
    writeTVar (ds^.pos) p
    writeTVar (ds^.trackPos) trackPos'
{-# INLINE updatePos #-}

------------------------------------------------------------------------------
fanoutSpikeToCells :: DecoderState -> TrodeName -> PlaceCellTrode ->
                      Position -> Field -> TrodeSpike -> Maybe Handle -> IO ()
fanoutSpikeToCells ds trodeName trode pos trackPos spike p =
 -- H.timeAction (ds^.encodeProf) $ do
 do
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
--      when (doLog && (floor (utctDayTime tNow) `mod` 100 == 0)) $
--        BS.appendFile "spikes.txt" (toBS spike tNow)
      when (isJust p) $ maybe (return ()) (flip BS.hPutStrLn (toBS spike tNow tNow2)) p
--      when (doLog && (floor (utctDayTime tNow) `mod` 20 == 0)) $
--        modifyMVar (ds^.logData) (flip BS.append (toBS spike tNow))
{-# INLINE fanoutSpikeToCells #-}

toBS :: TrodeSpike -> UTCTime -> UTCTime -> BS.ByteString
toBS spike tNow tNow2 =
  BS.concat [(BS.pack . show . spikeTime) spike
            , ", "
            , (BS.filter (/= 's') . BS.take 9 . BS.pack
               . show . utctDayTime) tNow
            , ", "
            , (BS.filter (/= 's') . BS.take 9 . BS.pack
               . show . utctDayTime) tNow2]


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
clusterlessAddSpike :: DecoderState -> TrodeName -> Position -> Field
                    -> TrodeSpike -> Maybe Handle -> IO ()
clusterlessAddSpike ds tName p tPos spike log =
  case Map.lookup tName (ds^.trodes._Clusterless) of
    Nothing -> putStrLn "Orphan spike"
    Just t' -> H.timeAction (ds^.encodeProf) $ do
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
makeCPoint :: TrodeSpike -> Field -> ClusterlessPoint
makeCPoint spike tPos = ClusterlessPoint {
    _pAmplitude = U.convert . spikeAmplitudes $ spike
  , _pWeight    = 1
  , _pField     = normalize $ gt0 tPos
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
        -- if trode doesn't exist, make a new one.  there's no history,
        -- so make an empty history
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
        Nothing -> PlaceCell cMethod
                   (V.replicate (V.length $ allTrackPos track) 0)
        -- Found that trode, build cell to that name from history
        Just (PlaceCellTrode dpc sHist) ->
          PlaceCell cMethod
          (V.replicate (V.length $ allTrackPos track) 0)
          -- TODO: Build from spike history!


------------------------------------------------------------------------------
orderClusters :: FilePath -> FilePath -> IO ()
orderClusters cFile ttFile = do
  let trodeName = Text.unpack $ mwlTrodeNameFromPath ttFile
  cExists <- doesFileExist cFile
  when cExists $ do
    clusters' <- getClusters cFile ttFile
    case clusters' of
      Left _         -> return ()
      --Right clusts -> atomically . writeTQueue queue $
      --                (ArteMessage 0 "" Nothing (Request $ TrodeSetAllClusters (read trodeName) clusts))
      Right _ -> error "Got rid of ArteMessage"
  -- TODO: safeRead instead


endsIn :: FilePath -> String -> Bool
endsIn fp str = take (length str) (reverse fp) == reverse str

interpretPos :: DecoderArgs -> Socket -> Producer Position IO ()
interpretPos DecoderArgs{..} sock = case psPosFormat posSource of
  PosFormatOat  -> let posProducerNoHistory = udpJsonProducer 9000 sock
                   in  posProducerNoHistory >-> P.map unOatPosition >-> P.tee P.print >-> producePos
  PosFormatArtE -> udpSocketProducer 9000 sock

newtype OatPosition = OatPosition { unOatPosition :: Position }

instance A.FromJSON OatPosition where
  parseJSON (A.Object v) = do
    posConf         <- oatPosConvert <$> v .: "pos_ok"
    [pos_x,pos_y]   <- v .: "pos_xy"
    [vel_x,vel_y]   <- v .: "vel_xy"
    return . OatPosition $ (Position (-200) (Location pos_x pos_y 0) (Angle 0 0 0)
            (atan2 vel_y vel_x) (-300) posConf [] [] (-400) (Location 0 0 0))

oatPosConvert True = ConfSure
oatPosConvert False = ConfUnsure

    -- {"ID":"NA"
    -- ,"unit":0
    -- ,"pos_ok":true
    -- ,"pos_xy":[-393085.1284289367,-32104.866198798318]
    -- ,"vel_ok":true
    -- ,"vel_xy":[-86.56935814544997,-35.44021106250909]
    -- ,"head_ok":false
    -- ,"reg_ok":false
    -- }
