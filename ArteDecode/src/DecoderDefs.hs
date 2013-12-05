{-# LANGUAGE TemplateHaskell #-}

module DecoderDefs where

import Data.Ephys.EphysDefs
import Data.Ephys.Spike
import Data.Ephys.PlaceCell

import qualified Data.Map as Map
import Control.Lens
import Control.Concurrent.MVar

-- Placeholder.  Will be more like: KdTree (Vector Voltage) (Field Double)
type SpikeHistory = Int 

nullHistory :: SpikeHistory
nullHistory = 0

data DecodablePlaceCell = DecodablePlaceCell { _dpCell     :: PlaceCell
                                             , _dpCellTauN :: Int
                                             } deriving (Eq, Show)
$(makeLenses ''DecodablePlaceCell)

data PlaceCellTrode = PlaceCellTrode {
    _dUnits :: Map.Map PlaceCellName (MVar DecodablePlaceCell) 
  , _pcTrodeHistory :: SpikeHistory
  } deriving (Eq)
             
$(makeLenses ''PlaceCellTrode)

type NotClusts = [Int]  -- A placeholder, will be more like WeightedKdTree

data ClusterlessTrode = ClusterlessTrode { _dtNonClusts :: NotClusts
                                         , _dtTauN      :: [TrodeSpike]
                                         } deriving (Eq, Show)

data Trodes = Clusterless (Map.Map TrodeName (MVar ClusterlessTrode))
            | Clustered   (Map.Map TrodeName PlaceCellTrode)

$(makeLenses ''Trodes)
$(makePrisms ''Trodes)