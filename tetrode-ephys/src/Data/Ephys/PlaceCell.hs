{-|
Module      : Data.Ephys.PlaceCell
Description : Representation of hippocampal place cell receptive fields
Copyright   : (c) 2015 Greg Hale, Shea Levy
License     : BSD3
Maintainer  : imalsogreg@gmail.com
Stability   : experimental
Portability : GHC, Linux
-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Ephys.PlaceCell where

import Data.Ephys.Spike
import Data.Ephys.TrackPosition
import Data.Ephys.Cluster

import Control.Lens

-- | A single place cell
data PlaceCell = PlaceCell { -- | Region in voltage space
                             _cluster    :: ClusterMethod
                             -- | The number of spikes in each bin of the track
                             --   from this cell
                           , _countField :: Field
                           } deriving (Eq, Show)

$(makeLenses ''PlaceCell)


--TODO: Should this function test that spike is in cluster?
-- Or should the caller do that?
-- (called by: ArteDecode.hs, watchfields)
-- | Update a place cell with a new spike based on its current position
stepField :: PlaceCell -- ^ The original cell
          -> Field -- ^ The track position
          -> TrodeSpike -- ^ The spike
          -> PlaceCell -- ^ The updated cell
stepField cell currentPos spike =
  case spikeInCluster (cell^.cluster) spike of
    False -> cell
    True  -> cell & countField %~ (updateField (+) currentPos)

-- | The spiking rate in each bin in spikes/second
placeField :: PlaceCell -- ^ The cell of interest
           -> Field -- ^ The occupancy time of each bin
           -> Field
placeField cell occupancyField =
  updateField (/) (cell^.countField) occupancyField
