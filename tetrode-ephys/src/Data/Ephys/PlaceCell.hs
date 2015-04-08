{-# LANGUAGE TemplateHaskell #-}

module Data.Ephys.PlaceCell where

import Data.Ephys.Spike
import Data.Ephys.TrackPosition
import Data.Ephys.Cluster

import Control.Lens

data PlaceCell = PlaceCell { _cluster    :: ClusterMethod
                           , _countField :: Field
                           } deriving (Eq, Show)

$(makeLenses ''PlaceCell)


--TODO: Should this function test that spike is in cluster?
-- Or should the caller do that?
-- (called by: ArteDecode.hs, watchfields)
stepField :: PlaceCell -> Field -> TrodeSpike -> PlaceCell
stepField cell currentPos spike =
  case spikeInCluster (cell^.cluster) spike of
    False -> cell
    True  -> cell & countField %~ (updateField (+) currentPos)

placeField :: PlaceCell -> Field -> Field
placeField cell occupancyField =
  updateField (/) (cell^.countField) occupancyField