{-# LANGUAGE TemplateHaskell #-}

module DecoderDefs where

import Data.Ephys.EphysDefs
import Data.Ephys.Spike
import Data.Ephys.PlaceCell

import qualified Data.Map as Map
import Control.Lens
import Control.Concurrent.STM

-- Placeholder.  Will be more like: KdTree (Vector Voltage) (Field Double)
type SpikeHistory = Int 

nullHistory :: SpikeHistory
nullHistory = 0

data DecodableUnit = DecodableUnit { _dpCell     :: PlaceCell
                                   , _dpCellTauN :: Int
                             } deriving (Eq, Show)
$(makeLenses ''DecodableUnit)

type PlaceCellTrode = Map.Map PlaceCellName (TVar DecodableUnit)

type NotClusts = Int  -- A placeholder, will be more like WeightedKdTree

data DecodableTrode = DecodableTrode { _dtNonClusts :: NotClusts
                                     , _dtTauN      :: [TrodeSpike]
                                     } deriving (Eq, Show)

data Trodes = Clusterless (Map.Map TrodeName (TVar DecodableTrode))
            | Clustered
             (Map.Map TrodeName PlaceCellTrode)
