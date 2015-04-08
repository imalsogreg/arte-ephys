{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable        #-}

module Data.Ephys.Spike where

import Data.Ephys.Timeseries.Filter
import Data.Ord (comparing)
import Data.Text hiding (zip, map,foldl1')
import Data.Text.Encoding
import Data.Time
import Control.Monad (liftM)
import qualified Data.Serialize as S
import qualified Data.Binary as B
import           Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Vector.Cereal
import Data.Vector.Binary
import Data.Ephys.EphysDefs


------------------------------------------------------------------------------
type Waveform = U.Vector Voltage  -- This should be the waveform from Data.Ephys.Waveform probably?


------------------------------------------------------------------------------
-- |Representation of an action potential recorded on a tetrode
data TrodeSpike = TrodeSpike { spikeTrodeName      :: !Int
                             , spikeTrodeOptsHash  :: !Int
                             , spikeTime           :: !ExperimentTime
                             , spikeWaveforms      :: V.Vector Waveform
                             }
                  deriving (Eq, Show, Typeable)


------------------------------------------------------------------------------
spikePeakIndex :: TrodeSpike -> Int
spikePeakIndex s =
  let chanMaxIs   = V.map U.maxIndex (spikeWaveforms s) :: V.Vector Int
      chanMax     = V.map U.maximum  (spikeWaveforms s) :: V.Vector Voltage
      chanWithMax = V.maxIndex chanMax                  :: Int
  in chanMaxIs V.! chanWithMax

------------------------------------------------------------------------------
spikeWidth :: TrodeSpike -> Int
spikeWidth s =
  let iChanMax       = V.maxIndexBy (comparing U.maximum) (spikeWaveforms s)
  in  chanSpikeWidth $ spikeWaveforms s V.! iChanMax

                       
------------------------------------------------------------------------------
chanSpikeWidth :: U.Vector Voltage -> Int
chanSpikeWidth vs =
  let tailPts = U.drop (U.maxIndex vs + 1) vs
      iTailMin                             -- Index of valley after peak
        | U.null tailPts = 0
        | otherwise      = 1 + U.minIndex tailPts
  in iTailMin
  

------------------------------------------------------------------------------
chanPeakIndexAndV :: U.Vector Voltage -> (Int,Voltage)
chanPeakIndexAndV vs = U.foldl1' maxBySnd $ U.zip (U.fromList [0..nSamps]) vs
  where nSamps = U.length vs


------------------------------------------------------------------------------
maxBySnd :: Ord b => (a,b) -> (a,b) -> (a,b)
maxBySnd a@(_,v) b@(_,v') = if v > v' then a else b


------------------------------------------------------------------------------
spikeAmplitudes  :: TrodeSpike -> V.Vector Voltage
spikeAmplitudes s = V.map (U.! i) (spikeWaveforms s)
  where i = spikePeakIndex s


------------------------------------------------------------------------------
-- |Representation of tetroe-recorded AP features
data SpikeModel = SpikeModel { mSpikeTime          :: ExperimentTime
                             , mSpikePeakAmp       :: U.Vector Voltage
                             , mSpikepPeakToTroughT :: DiffTime
                             , mSpikepPeakToTroughV :: U.Vector Voltage
                             } deriving (Show)
-- TODO: Do I need the rest of the mwl params?  maxwd? maxh?
-- What about things like 'noise'?  Or 'deviation from the cluster'?


------------------------------------------------------------------------------
-- |Polar coordinates representation of tetrode-recorded AP
data PolarSpikeModel = PolarSpikeModel { pSpikeTime      :: ExperimentTime
                                       , pSpikeMagnitute :: Voltage
                                       , pSpikeAngles    :: U.Vector Double
                                       } deriving (Show)


------------------------------------------------------------------------------
-- This should be part of arte, not tetrode-ephys?  It's about recording
-- But I need it to decode files...
data TrodeAcquisitionOpts = TrodeAcquisitionOpts { spikeFilterSpec :: FilterSpec
                                                 , spikeThresholds :: [Voltage]
                                                 } deriving (Eq, Show)


------------------------------------------------------------------------------
toRelTime :: TrodeSpike -> Double
toRelTime TrodeSpike{..} = spikeTime

------------------------------------------------------------------------------
instance S.Serialize TrodeSpike where
  put TrodeSpike{..} = do
    S.put spikeTrodeName
    S.put spikeTrodeOptsHash
    S.put spikeTime
    S.put spikeWaveforms
  get = do
    name <- S.get
    opts <- S.get
    time <- S.get
    waveforms <- S.get
    return $ TrodeSpike name opts time waveforms

------------------------------------------------------------------------------
instance B.Binary TrodeSpike where
  put TrodeSpike{..} = do
    B.put spikeTrodeName
    B.put spikeTrodeOptsHash
    B.put spikeTime
    B.put spikeWaveforms
  get = do
    name <- B.get
    opts <- B.get
    time <- B.get
    waveforms <- B.get
    return $ TrodeSpike name opts time waveforms


------------------------------------------------------------------------------
-- TODO: a test spike
mySpike :: IO TrodeSpike
mySpike = return $ TrodeSpike tName tOpts sTime sWF
  where tName = 63
        tOpts = 1001
        sTime = 10.10
        sWF = V.replicate 4 $ U.replicate 32 0
        
