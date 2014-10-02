{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Arte.Decode.Config where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.STM.TVar
import           Control.Lens
import qualified Data.Map.Strict                as Map
import           Data.Time.Clock
import qualified Data.Vector                    as V
------------------------------------------------------------------------------
import           Data.Ephys.EphysDefs
import           Data.Ephys.Position            (Angle (..), Location (..),
                                                 PosConf (..), Position (..))
import           Data.Ephys.TrackPosition       (Field, PosKernel (..),
                                                 Track (..), TrackPos (..),
                                                 allTrackPos, circularTrack)
import           System.Arte.Decode.Types
import           System.Arte.Decode.Histogram


------------------------------------------------------------------------------
-- TODO: Make decode general on tracks and kernels.
defTrack :: Track
defTrack = circularTrack (0,0) 0.57 0.5 0.25 0.3
kernel :: PosKernel
--kernel = PosDelta
kernel  = PosGaussian 0.2

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
initialState :: DecoderArgs -> IO DecoderState
initialState DecoderArgs{..} = do
  let zeroField = V.replicate (V.length $ allTrackPos defTrack) 0
      p0        = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0
                  ConfSure sZ sZ (-1/0 :: Double) (Location 0 0 0)
      sZ        = take 15 (repeat 0)
      clusts    = if clusterless
                  then clistTrodes $ Clusterless Map.empty
                  else clistTrodes $ Clusterless Map.empty
  t0       <- getCurrentTime
  DecoderState <$>
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
    <*> pure (\t -> startExperimentTime + realToFrac (diffUTCTime t t0))
    <*> pure Nothing
    <*> newTVarIO (mkHistogram (0,0.005) 20)
    <*> newTVarIO (mkHistogram (0,0.05) 20)


------------------------------------------------------------------------------
clistTrodes :: Trodes -> TrodeDrawOptions
clistTrodes (Clustered tMap) =
  (map f $ Map.toList tMap) ++
  [[DrawOccupancy], [DrawDecoding]]
    where
      f :: (TrodeName, PlaceCellTrode) -> [TrodeDrawOption]
      f (tName, PlaceCellTrode units _) = map (\(n,u) -> DrawPlaceCell n u)
                                          (Map.toList units)
clistTrodes (Clusterless tMap) =
  (map f $ Map.toList tMap)
  ++ [[DrawOccupancy], [DrawDecoding] ]
  where
    f :: (TrodeName,TVar ClusterlessTrode) -> [TrodeDrawOption]
    f (n,t) = [ DrawClusterless n t
                (ClessDraw (XChan x) (YChan y))
              | x <- [0  ..3]
              , y <- [x+1..3]
              ]
