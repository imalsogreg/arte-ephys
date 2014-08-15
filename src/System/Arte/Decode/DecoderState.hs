{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Arte.Decode.DecoderState where

------------------------------------------------------------------------------
import System.Arte.Decode.DecoderDefs    (Trodes(..),ClusterlessTrode(..),DecodablePlaceCell(..))
import Data.Ephys.Position               (Position(..), Location(..), Angle(..), PosConf(..))
import Data.Ephys.TrackPosition          (Field, Track(..), TrackPos(..), PosKernel(..), allTrackPos, circularTrack)
import Data.Ephys.EphysDefs
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Control.Concurrent.STM.TVar
import qualified Data.ByteString.Char8       as BS
import           Data.Time.Clock
import qualified Data.CircularList           as CL
import qualified Data.Map.Strict             as Map
import qualified Data.Vector                 as V


------------------------------------------------------------------------------
data DecoderState = DecoderState
                    { _pos           :: TVar Position
                    , _trackPos      :: TVar (Field)
                    , _occupancy     :: TVar (Field)
                    , _maybeunused   :: TVar (Field) 
                    , _trodes        :: Trodes
                    , _decodedPos    :: TVar (Field)
                    , _trodeDrawOpt  :: TrodeDrawOptions
                    , _trodeInd      :: Int
                    , _clustInd      :: Int
                    , _drawKDESample :: Bool
                    , _toExpTime     :: UTCTime -> Double
                    }


------------------------------------------------------------------------------
data TrodeDrawOption =
    DrawPlaceCell   PlaceCellName (TVar DecodablePlaceCell)
  | DrawClusterless TrodeName     (TVar ClusterlessTrode) ClessDraw
  | DrawOccupancy
  | DrawDecoding
  | DrawError String
  deriving (Eq)


------------------------------------------------------------------------------
newtype XChan = XChan Int deriving (Eq)
newtype YChan = YChan Int deriving (Eq)
newtype ClessPoint = ClessPoint (Double,Double,Double,Double)
                    deriving (Eq)


------------------------------------------------------------------------------
data ClessDraw = ClessDraw XChan YChan (Maybe ClessPoint)
  deriving (Eq)


------------------------------------------------------------------------------
instance Show TrodeDrawOption where
  show (DrawPlaceCell n _)     = "DrawPlaceCell " ++ show n
  show (DrawClusterless n _ _) = "DrawClusterless " ++ show n
  show  DrawOccupancy          = "DrawOccupancy"
  show  DrawDecoding           = "DrawDecoding"
  show (DrawError s)           = "DrawError " ++ s

type TrodeDrawOptions = CL.CList (CL.CList TrodeDrawOption)


------------------------------------------------------------------------------
-- TODO: Make decode general on tracks and kernels.  
track :: Track
track = circularTrack (0,0) 0.57 0.5 0.25 0.2
kernel :: PosKernel
--kernel = PosDelta
kernel  = PosGaussian 0.05

------------------------------------------------------------------------------
trackBins0 :: V.Vector TrackPos
trackBins0 = allTrackPos track


------------------------------------------------------------------------------
emptyField :: Field
emptyField = let l = V.length trackBins0
             in  V.replicate l (1 / fromIntegral l)


------------------------------------------------------------------------------
$(makeLenses ''DecoderState)