{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Arte.Decode.DecoderDefs where

------------------------------------------------------------------------------
import           Control.Concurrent.STM.TVar
import           Control.Lens
import qualified Data.Map.Strict as Map
import           Data.Monoid
import qualified Data.Vector.Unboxed as U
import           System.Console.CmdArgs
------------------------------------------------------------------------------
import Data.Ephys.EphysDefs
import Data.Ephys.TrackPosition
import Data.Ephys.Spike
import Data.Ephys.PlaceCell
import Data.Map.KDMap



------------------------------------------------------------------------------
-- Placeholder.  Will be more like: KdTree (Vector Voltage) (Field Double)
type SpikeHistory = Int 

nullHistory :: SpikeHistory
nullHistory = 0

runningThresholdSpeed :: Double
runningThresholdSpeed = 0.15;

data DecodablePlaceCell = DecodablePlaceCell { _dpCell     :: !PlaceCell
                                             , _dpCellTauN :: !Int
                                             } deriving (Eq, Show)
$(makeLenses ''DecodablePlaceCell)

data PlaceCellTrode = PlaceCellTrode {
    _dUnits :: Map.Map PlaceCellName (TVar DecodablePlaceCell) 
  , _pcTrodeHistory :: !SpikeHistory
  } deriving (Eq)
             
$(makeLenses ''PlaceCellTrode)

type NotClusts = KDMap Point4 ClusterlessPoint

data ClusterlessTrode = ClusterlessTrode { _dtNonClusts :: NotClusts
                                         , _dtTauN      :: [TrodeSpike]
                                         } deriving (Eq, Show)

data Trodes = Clusterless (Map.Map TrodeName (TVar ClusterlessTrode))
            | Clustered   (Map.Map TrodeName PlaceCellTrode)


------------------------------------------------------------------------------
-- Field is in the Key, not the Payload, because it needs to be
-- weighted-summed, not just summed, and payload append doesn't
-- consider the weights (althought it should - so, TODO refactor KDMap
-- add a KDValue class with weightedAppend method)
data ClusterlessPoint = ClusterlessPoint {
    pAmplitude  :: U.Vector Voltage
  , pWeight     :: !Double
  , pField      :: Field Double
  } deriving (Eq, Show)


------------------------------------------------------------------------------
instance Monoid ClusterlessPoint where
  mempty = ClusterlessPoint (U.fromList []) 0 (Map.empty)
  a `mappend` b = ClusterlessPoint
                  (U.zipWith (weightedSum) (pAmplitude a) (pAmplitude b))
                  (pWeight a + pWeight b)
                  (Map.unionWith (weightedSum) (pField a) (pField b))
    where
      weightedSum x y  = let wA = pWeight a
                             wB = pWeight b
                             wR =1/ (wA + wB)
                         in  (x*wA + y*wB) * wR
        
instance KDKey ClusterlessPoint where
  pointD p i  = pAmplitude p U.! (fromIntegral i)
  pointSize p = fromIntegral . U.length $ pAmplitude p
  pointW p    = realToFrac $ pWeight p
  dSucc p d   = succ d `mod` pointSize p
  dPred p d   = pred d `mod` pointSize p

------------------------------------------------------------------------------
newtype MostRecentTime = MostRecentTime { unMostRecentTime :: Double}
                         deriving (Eq,Ord,Show,Num,Real)

instance Monoid MostRecentTime where
  mempty = MostRecentTime (-1/0)
  a `mappend` b = max a b

$(makeLenses ''Trodes)
$(makePrisms ''Trodes)


------------------------------------------------------------------------------
data DecoderArgs = DecoderArgs {mwlBaseDirectory    :: FilePath
                               ,startExperimentTime :: Double
                               ,doLogging           :: Bool
                               ,clusterless         :: Bool
                               }
                 deriving (Show,Data,Typeable)
decoderArgs :: DecoderArgs
decoderArgs = DecoderArgs { mwlBaseDirectory =
                             "" &= 
                             help "Data directory when not using network data"
                          , startExperimentTime =
                            0 &=
                            help "Start time when spooling from disk directly"
                          , doLogging =
                            False &= help "Commit data to log files"
                          , clusterless =
                            False &= help "Perform clusterless decoding"
                          }
