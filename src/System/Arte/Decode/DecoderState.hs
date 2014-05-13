{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Arte.Decode.DecoderState where

import System.Arte.Decode.DrawingHelpers
import System.Arte.Decode.DecoderDefs
import Data.Ephys.Position
import Data.Ephys.TrackPosition

import Control.Applicative
import qualified Data.Map.Strict as Map
import Control.Lens
import Control.Concurrent.STM.TVar
import qualified Data.ByteString.Char8 as BS
import qualified Data.CircularList as CL
  
data DecoderState = DecoderState
                    { _pos            :: TVar Position
                    , _trackPos       :: TVar (Field Double)
                    , _occupancy      :: TVar (Field Double)
                    , _maybeunused    :: TVar (Field Double) 
                    , _trodes         :: Trodes
                    , _decodedPos     :: TVar (Field Double)
                    , _trodeDrawOpt   :: TrodeDrawOptions
--                    , _logData        :: TVar BS.ByteString
                    }

$(makeLenses ''DecoderState)

-- TODO: Make decode general on tracks and kernels.  
track :: Track
track = circularTrack (0,0) 0.57 0.5 0.25 0.1
kernel :: PosKernel
--kernel = PosDelta
kernel  = PosGaussian 0.05

initialState :: IO DecoderState
initialState = do
  let zeroField = Map.fromList [(tp,0.1) | tp <- allTrackPos track]
      p0 = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0 ConfSure sZ sZ (-1/0 :: Double) (Location 0 0 0)
      sZ = take 15 (repeat 0)
  DecoderState <$>
    newTVarIO p0
    <*> newTVarIO zeroField
    <*> newTVarIO zeroField
    <*> newTVarIO zeroField
    <*> return (Clustered Map.empty)
    <*> newTVarIO zeroField
    <*> pure (clistTrodes (Clustered Map.empty)) 
--    <*> newTVarIO ""
