{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Arte.Decode.Config where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Lens
import qualified Data.Map.Strict                as Map
import           Data.Time.Clock
import qualified Data.Vector                    as V
import           Data.Map.KDMap
------------------------------------------------------------------------------
import           Data.Ephys.EphysDefs
import           Data.Ephys.Position            (Angle (..), Location (..),
                                                 PosConf (..), Position (..))
import           Data.Ephys.TrackPosition       (Field, PosKernel (..),
                                                 Track (..), TrackPos (..),
                                                 allTrackPos, circularTrack,
                                                 radialArmMaze)
import           System.Arte.Decode.Types
import           System.Arte.Decode.Histogram


------------------------------------------------------------------------------
-- TODO: Make decode general on tracks and kernels.
defTrack :: Track
--defTrack = circularTrack (0,0) 0.57 0.5 0.25 0.2
defTrack = radialArmMaze (0,0) 0 0.2 8 1.5 0.5 0.1 0.1
kernel :: PosKernel
--kernel = PosDelta
kernel  = PosGaussian 0.15


------------------------------------------------------------------------------
trackBins0 :: Track -> V.Vector TrackPos
trackBins0 t = allTrackPos t


------------------------------------------------------------------------------
emptyField :: Track -> Field
emptyField track = let l = V.length $ trackBins0 track
                   in  V.replicate l (1 / fromIntegral l)

zerosField :: Track -> Field
zerosField track = let l = V.length $ trackBins0 track
                   in  V.replicate l 0

------------------------------------------------------------------------------
$(makeLenses ''DecoderState)


------------------------------------------------------------------------------
pos0 :: Position
pos0 = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0
       ConfSure sZ sZ (-1/0 :: Double) (Location 0 0 0)
  where sZ = take 15 (repeat 0)

field0 :: Field
field0 = V.replicate (V.length $ allTrackPos defTrack) 0

------------------------------------------------------------------------------
initialState :: DecoderArgs -> IO DecoderState
initialState DecoderArgs{..} = do
  newTrode <- case clusterless of
                 False -> return . Clustered $
                          PlaceCellTrode Map.empty nullHistory
                 True -> Clusterless <$>
                         (newTVarIO $ ClusterlessTrode KDEmpty [])
  t0       <- getCurrentTime
  DecoderState <$>
    newTVarIO pos0
    <*> newTVarIO field0
    <*> newTVarIO field0
    <*> return newTrode
    <*> newTVarIO field0
    <*> pure (clistTrodes newTrode)
    <*> pure 0
    <*> pure 0
    <*> pure False
    <*> pure (\t -> startExperimentTime + realToFrac (diffUTCTime t t0))
    <*> pure Nothing
    <*> atomically (mkHistogram (0,0.005) 20)
    <*> atomically (mkHistogram (0,0.05) 20)


------------------------------------------------------------------------------
clistTrodes :: Trode -> TrodeDrawOptions
clistTrodes (Clustered (PlaceCellTrode units _)) =
  map (\(n,u) -> DrawPlaceCell n u) (Map.toList units) ++ [DrawOccupancy, DrawDecoding]
clistTrodes (Clusterless t) =
  [ DrawClusterless t (ClessDraw (XChan x) (YChan y)) | x <- [0..3], y <- [x+1..3] ] ++  [DrawOccupancy, DrawDecoding]
