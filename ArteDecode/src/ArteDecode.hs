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
import Data.Ephys.GlossPictures

import Control.Applicative ((<$>),(<*>),pure)
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
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

----------------------------------------
-- TODO: There is way too much STM here
-- UI timing here is handled by gloss,
-- So decoderState top-level fields
-- probably don't need to be in TVars.
-- Also ought to be using lens right
-- here...
---------------------------------------

-- Placeholder.  Will be more like: KdTree (Vector Voltage) (Field Double)
type SpikeHistory = Int 

nullHistory :: SpikeHistory
nullHistory = 0

data DecodableUnit = DecodableUnit { _dpCell     :: PlaceCell
                                   , _dpCellTauN  :: Int
                             } deriving (Eq, Show)
$(makeLenses ''DecodableUnit)

type NotClusts = Int  -- A placeholder, will be more like WeightedKdTree

data DecodableTrode = DecodableTrode { _dtNonClusts :: NotClusts
                                     , _dtTauN      :: [TrodeSpike]
                                     } deriving (Eq, Show)

data Trodes = Clusterless (Map.Map TrodeName (TVar DecodableTrode))
            | Clustered
             (Map.Map TrodeName DecodableUnit)

type PlaceCellTrodes = Map.Map PlaceCellName (TVar DecodableUnit)

data TrodeDrawOption = DrawPlaceCell TrodeName PlaceCellName
                     | DrawClusterless (Maybe TrodeName)
                     | DrawOccupancy
                     | DrawDecoding

{-
trodeLists :: Trodes -> Either [TrodeName] [[(TrodeName,PlaceCellName)]]
trodeLists (Clusterless tMap) = Map.keys tMap
trodeLists (Clustered   tMap) = map (\(tn,t) -> [(tn,pcn)|pcn <- Map.keys t ])
                                Map.toList tMap
-}

trodeNames :: Trodes -> [TrodeName]
trodeNames (Clusterless tMap) = Map.keys tMap
trodeNames (Clustered   tMap) = Prelude.map fst $ Map.keys tMap

cellNames :: Trodes -> TrodeName -> Maybe [PlaceCellName]
cellNames trodes tName = Map.keys `fmap` Map.lookup tName trodes

nextTrode :: Trodes -> TrodeDrawOption -> Bool -> TrodeDrawOption
nextTrode trodes opt revFlag = case opt of
  DrawDecoding  = DrawPlaceCell trode0 cell0
  DrawOccupancy = DrawDecoding
  DrawPlaceCell tName cName 
    | tName == last (trodeNames trodes) = undefined
    | otherwise = case tName' of
    Just tName -> head . drop 1 . dropWhile (/= tName) $
                  (trodeNames trodes)

prevTrode :: Either [TrodeName] [[(TrodeName,PlaceCellName)]] ->
             Maybe TrodeName -> Maybe TrodeName
prevTrode trodes tName'
  | tName' == Nothing              = last (trodeNames trodes)
  | tName' == (Just $ head (trodeNames trodes)) = Nothing
  | otherwise                      =
    head [n | n <- (trodeNames trodes),
          nextTrode trodes (Just n) == tName']

nextCell :: Trodes -> TrodeDrawOption -> Bool -> TrodeDrawOption
nextCell (Clusterless _) = id 
nextCell trodes opt revFlag = case opt of
  DrawOccupancy -> opt
  DrawDecoding  -> opt
  DrawPlaceCell tName cName ->
    let rFun = if revFlag then reverse else id in
    let cName' = case cellNames trodes of
          Nothing -> opt
          Just cNames -> case dropWhile (/= cName) (rFun cNames) of
            []   -> head cellNames
            a:[] -> head cellNames
            a:bs -> head bs
    in DrawPlaceCell tName cName'
            
  

stepDrawOpt :: Trodes ->
               SpecialKey -> TrodeDrawOption -> TrodeDrawOption
stepDrawOpt tMap k DrawOccupancy
  | k == KeyRight = DrawDecoding
  | k == KeyLeft  = DrawPlaceCell (Just $ last (trodeNames tMap)) Nothing
  | otherwise     = DrawOccupancy
stepDrawOpt tMap k DrawDecoding
  | k == KeyRight = DrawPlaceCell (Just $ head (trodeNames tMap)) Nothing
  | k == KeyLeft  = DrawOccupancy
  | otherwise     = DrawDecoding
stepDrawOpt tMap k opt@(DrawPlaceCell tName' pcName')
  | k == KeyRight = maybe DrawOccupancy (\a -> DrawPlaceCell a Nothing) (nextTrode $ trodeLists tMap tName')
  | k == KeyLeft = maybe DrawDecoding (\a -> DrawPlaceCell a Nothing) (nextTrode $ trodeLists tMap tName')
  | k == KeyDown = maybe (\p -> DrawPlaceCell tName' p) (nextCell trodeLists tMap tName' pcName')
  | k == KeyUp  = maybe (\p -> DrawPlaceCell tName' p) (prevCell trodeLists tMap tName' pcName')


data DecoderState = DecoderState { _pos          :: TVar Position
                                 , _trackPos     :: TVar (Field Double)
                                 , _occupancy    :: TVar (Field Double)
                                 , _lastEstimate :: TVar (Field Double) --unused?
                                 , _trodes       :: Trodes
                                 , _decodedPos   :: TVar (Field Double)
                                 , _trodeDrawOpt :: TrodeDrawOption
                                 }

$(makeLenses ''DecoderState)

--TODO: This is also pretty bad
draw :: DecoderState -> IO Picture
draw ds = do
  pos  <-  readTVarIO $ ds^.pos
  occ  <- readTVarIO  $  ds^.occupancy
  trs  <- readTVarIO $ ds^.trodes
  dPos <- readTVarIO $ ds^. decodedPos
  let trackPicture = drawTrack track
      posPicture = drawPos pos
      t = ds^.trodeDrawOpt
  fieldPicture <- fieldPicture' dPos occ trs t
  return $ pictures [trackPicture,fieldPicture,posPicture]
    where
      fieldPicture' :: Field Double -> Field Double -> Map.Map Int Trode -> Int -> IO Picture
      fieldPicture' dPos occ trs t
        | t == length (Map.toList trs) = case ds^.subDrawInd of
          0 -> return $ drawField dPos
          1 -> return $ drawField occ
          _ -> error "subDrawInd should only be 0 or 1"
        | t < length (Map.toList trs) = do
          let trode = (Map.elems trs) !! t :: Trode
          placeCellMap <- readTVarIO (fst trode) :: IO (Map.Map Int PlaceCell)
          when (ds^.subDrawInd >= Map.size placeCellMap)
            (error "Bad index into placeCellMap")
          let placeCell = (Map.elems placeCellMap) !! (ds ^. subDrawInd)
              pf = placeField placeCell occ
          return . drawField $ pf
        | otherwise = error "Bad trode requested"


initialState :: IO DecoderState
initialState = atomically $ do
  let zeroField = Map.fromList [(tp,0) | tp <- allTrackPos track]
      p0 = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0 ConfSure sZ sZ (-1/0 :: Double) (Location 0 0 0)
      sZ = take 15 (repeat 0)
  DecoderState <$>
    newTVar p0 <*>
    newTVar zeroField <*>
    newTVar zeroField <*>
    newTVar zeroField <*>
    newTVar Map.empty <*>
    newTVar zeroField <*>
    pure 0 <*>
    pure 0

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
              white 30 ds draw inputsIO stepIO 
            mapM_ wait subAs
            wait subP 
            print "Past wait subAs"

inputsIO :: Event -> DecoderState -> IO DecoderState
inputsIO e ds =
  case e of
    EventMotion _ -> return ds
    EventKey (SpecialKey KeyDown) Down _ _ ->
      trodeIndAdvance ds 1
    EventKey (SpecialKey KeyUp) Down _ _ ->
      trodeIndAdvance ds (-1)
    EventKey (SpecialKey KeyRight) Down _ _  ->
      trodeSubAdvance ds 1
    EventKey (SpecialKey KeyLeft) Down _ _ ->
      trodeSubAdvance ds (-1)
    EventKey k Down _ _ ->
      putStrLn ("Ignoring keypress " ++ show k) >> return ds
    e -> putStrLn ("Ignoring event " ++ show e) >> return ds

stepIO :: Float -> DecoderState -> IO DecoderState
stepIO _ = return

trodeIndAdvance :: DecoderState -> Int -> IO DecoderState
trodeIndAdvance ds i = do
  trodeMap <- readTVarIO (ds^.trodes)
  let n = Map.size trodeMap
      i' = ds^.trodeDrawInd + i `mod` (n+1)
  print $ "setting ind to " ++ show i'
  return ds {_trodeDrawInd = i'
            ,_subDrawInd = 0 }

trodeSubAdvance :: DecoderState -> Int -> IO DecoderState
trodeSubAdvance ds i = do
  trodeMap <- readTVarIO (ds^.trodes)
  let nTrodes = (Map.size trodeMap)
  print nTrodes
  print i
  let i' = if (ds^.subDrawInd) == nTrodes
           then (return $ (ds^.subDrawInd + i) `mod` 1)
           else (do
                    let trode =
                          (Map.elems trodeMap) !!
                          (ds^.trodeDrawInd)
                    placeCellMap <- readTVarIO $ fst trode
                    let n = Map.size placeCellMap
                    return $ (ds^.subDrawInd + 1) `mod` n)
  i'' <- i' 
  putStrLn $ "Set subInd to " ++ show i''
  return $ ds { _subDrawInd = i'' }

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
