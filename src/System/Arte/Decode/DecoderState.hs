{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Arte.Decode.DecoderState where

------------------------------------------------------------------------------
import System.Arte.Decode.DecoderDefs    (Trodes(..),ClusterlessTrode(..),DecodablePlaceCell(..))
import Data.Ephys.Position               (Position(..), Location(..), Angle(..), PosConf(..))
import Data.Ephys.TrackPosition          (Field, Track(..), PosKernel(..), allTrackPos, circularTrack)
import Data.Ephys.EphysDefs
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Control.Concurrent.STM.TVar
import qualified Data.ByteString.Char8       as BS
import qualified Data.CircularList           as CL
import qualified Data.Map.Strict             as Map


------------------------------------------------------------------------------
data DecoderState = DecoderState
                    { _pos           :: TVar Position
                    , _trackPos      :: TVar (Field Double)
                    , _occupancy     :: TVar (Field Double)
                    , _maybeunused   :: TVar (Field Double) 
                    , _trodes        :: Trodes
                    , _decodedPos    :: TVar (Field Double)
                    , _trodeDrawOpt  :: TrodeDrawOptions
                    , _trodeInd      :: Int
                    , _clustInd      :: Int
                    , _drawKDESample :: Bool
                    }


------------------------------------------------------------------------------
data TrodeDrawOption = DrawPlaceCell   PlaceCellName (TVar DecodablePlaceCell)
                     | DrawClusterless (TVar ClusterlessTrode)
                     | DrawOccupancy
                     | DrawDecoding
                     | DrawError String
                     deriving (Eq)


------------------------------------------------------------------------------
instance Show TrodeDrawOption where
  show (DrawPlaceCell name _) = "DrawPlaceCell " ++ show name
  show (DrawClusterless _)    = "DrawClusterless"
  show  DrawOccupancy         = "DrawOccupancy"
  show  DrawDecoding          = "DrawDecoding"
  show (DrawError s)          = "DrawError " ++ s

type TrodeDrawOptions = CL.CList (CL.CList TrodeDrawOption)


------------------------------------------------------------------------------
-- TODO: Make decode general on tracks and kernels.  
track :: Track
track = circularTrack (0,0) 0.57 0.5 0.25 0.3
kernel :: PosKernel
--kernel = PosDelta
kernel  = PosGaussian 0.05

$(makeLenses ''DecoderState)