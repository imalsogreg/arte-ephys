{-# LANGUAGE TemplateHaskell #-}

module DecoderState where

import DrawingHelpers
import DecoderDefs
import Data.Ephys.Position
import Data.Ephys.TrackPosition

import Control.Applicative
import qualified Data.Map as Map
import Control.Lens
import Control.Concurrent.MVar
import qualified Data.CircularList as CL
  
data DecoderState = DecoderState
                    { _pos          :: MVar Position
                    , _trackPos     :: MVar (Field Double)
                    , _occupancy    :: MVar (Field Double)
                    , _lastEstimate :: MVar (Field Double) --unused?
                    , _trodes       :: Trodes
                    , _decodedPos   :: MVar (Field Double)
                    , _trodeDrawOpt :: TrodeDrawOptions
                    }

$(makeLenses ''DecoderState)

-- TODO: Make decode general on tracks and kernels.  
track :: Track
track = circularTrack (0,0) 0.57 0.5 0.25 0.15
kernel :: PosKernel
kernel  = PosGaussian 0.2

initialState :: IO DecoderState
initialState = do
  let zeroField = Map.fromList [(tp,0.1) | tp <- allTrackPos track]
      p0 = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0 ConfSure sZ sZ (-1/0 :: Double) (Location 0 0 0)
      sZ = take 15 (repeat 0)
  DecoderState <$>
    newMVar p0 <*>
    newMVar zeroField <*>
    newMVar zeroField <*>
    newMVar zeroField <*>
    return (Clustered Map.empty) <*>
    newMVar zeroField <*> 
    pure (clistTrodes (Clustered Map.empty))
