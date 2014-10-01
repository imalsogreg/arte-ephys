{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.Arte.Decode.Config where

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
import           System.Arte.Decode.Types
import           System.Arte.Decode.Histogram



                    
------------------------------------------------------------------------------
-- TODO: Make decode general on tracks and kernels.
track :: Track
track = circularTrack (0,0) 0.57 0.5 0.25 0.3
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

zerosField :: Field
zerosField = let l = V.length trackBins0
             in V.replicate l 0

------------------------------------------------------------------------------
$(makeLenses ''DecoderState)
