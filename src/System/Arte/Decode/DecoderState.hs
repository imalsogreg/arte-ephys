{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.Arte.Decode.DecoderState where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.STM.TVar
import           Control.Lens
import qualified Data.ByteString.Char8          as BS
import qualified Data.CircularList              as CL
import qualified Data.Map.Strict                as Map
import           Data.Time.Clock
import qualified Data.Vector                    as V
------------------------------------------------------------------------------
import           Data.Ephys.EphysDefs
import           Data.Ephys.Position            (Angle (..), Location (..),
                                                 PosConf (..), Position (..))
import           Data.Ephys.TrackPosition       (Field, PosKernel (..),
                                                 Track (..), TrackPos (..),
                                                 allTrackPos, circularTrack)
import           System.Arte.Decode.DecoderDefs 


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
                    , _samplePoint   :: Maybe ClusterlessPoint
                    }

                    
------------------------------------------------------------------------------
-- TODO: Make decode general on tracks and kernels.
track :: Track
track = circularTrack (0,0) 0.57 0.5 0.25 0.2
kernel :: PosKernel
--kernel = PosDelta
kernel  = PosGaussian 0.2

------------------------------------------------------------------------------
trackBins0 :: V.Vector TrackPos
trackBins0 = allTrackPos track


------------------------------------------------------------------------------
emptyField :: Field
emptyField = let l = V.length trackBins0
             in  V.replicate l (1 / fromIntegral l)


------------------------------------------------------------------------------
$(makeLenses ''DecoderState)
