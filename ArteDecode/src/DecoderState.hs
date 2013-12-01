{-# LANGUAGE TemplateHaskell #-}

module DecoderState where

import DrawingHelpers
import DecoderDefs
import Data.Ephys.Position
import Data.Ephys.TrackPosition

import Control.Applicative
import qualified Data.Map as Map
import Control.Lens
import Control.Concurrent.STM
import qualified Data.CircularList as CL
  
data DecoderState = DecoderState
                    { _pos          :: TVar Position
                    , _trackPos     :: TVar (Field Double)
                    , _occupancy    :: TVar (Field Double)
                    , _lastEstimate :: TVar (Field Double) --unused?
                    , _trodes       :: Trodes
                    , _decodedPos   :: TVar (Field Double)
                    , _trodeDrawOpt :: TrodeDrawOptions
                    }

$(makeLenses ''DecoderState)

-- TODO: Make decode general on tracks and kernels.  
track :: Track
track = circularTrack (0,0) 0.57 0.5 0.25 0.15
kernel :: PosKernel
kernel  = PosGaussian 0.2

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
    return (Clustered Map.empty) <*>
    newTVar zeroField <*>
    return CL.empty
