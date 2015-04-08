module Data.Ephys.PositionDecoding where

import Data.Ephys.PlaceCell
import Data.Ephys.TrackPosition

import qualified Data.Map as Map

-- This is done in arte for now... thought it should be here
estimatePosition :: Map.Map PlaceCell Double -- A map from a place cell (with
                                             -- its field built only from
                                             -- spikes occurring before the
                                             -- start of the reconstruction
                                             -- timebin
                 -> Field                    -- Pos PDF
estimatePosition = undefined