{-|
Module      : Data.Ephys.Spike
Description : Representations of neuron spikes
Copyright   : (c) 2015 Greg Hale, Shea Levy
License     : BSD3
Maintainer  : imalsogreg@gmail.com
Stability   : experimental
Portability : GHC, Linux
-}

{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable        #-}

module Data.Ephys.Spike where

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
-- | A short series of voltage readings for one channel of a trode around
--   the time of a spike
type Waveform = U.Vector Voltage  -- This should be the waveform from Data.Ephys.Waveform probably?


------------------------------------------------------------------------------
-- | Representation of an action potential recorded on a trode
data TrodeSpike = TrodeSpike { spikeTrodeName      :: !Int -- ^ Tetrode ID
                             -- | Time of the spike
                             , spikeTime           :: !ExperimentTime
                             -- | The waveforms of each trode channel
                             , spikeWaveforms      :: V.Vector Waveform
                             }
                  deriving (Eq, Show, Typeable)


------------------------------------------------------------------------------
-- | Find the time offset where the greatest voltage around a spike occurred.
spikePeakIndex :: TrodeSpike -- ^ The spike of interest
               -> Int
spikePeakIndex s =
  let chanMaxIs   = V.map U.maxIndex (spikeWaveforms s) :: V.Vector Int
      chanMax     = V.map U.maximum  (spikeWaveforms s) :: V.Vector Voltage
      chanWithMax = V.maxIndex chanMax                  :: Int
  in chanMaxIs V.! chanWithMax

------------------------------------------------------------------------------
-- | The number of samples between the peak and trough of the spike.
--   The width of the channel used for spike timing is used.
spikeWidth :: TrodeSpike -- ^ The spike of interest
           -> Int
spikeWidth s =
  let iChanMax       = V.maxIndexBy (comparing U.maximum) (spikeWaveforms s)
  in  chanSpikeWidth $ spikeWaveforms s V.! iChanMax

                       
------------------------------------------------------------------------------
-- | The number of samples between the peak and the trough of a single channel's
--   wave form.
chanSpikeWidth :: Waveform -- ^ The wave form of interest
               -> Int
chanSpikeWidth vs =
  let tailPts = U.drop (U.maxIndex vs + 1) vs
      iTailMin                             -- Index of valley after peak
        | U.null tailPts = 0
        | otherwise      = 1 + U.minIndex tailPts
  in iTailMin
  

------------------------------------------------------------------------------
-- | Find the maximum voltage and its offset of a single channel's wave form.
chanPeakIndexAndV :: Waveform -- ^ The wave form of interest.
                  -> (Int,Voltage)
chanPeakIndexAndV vs = U.foldl1' maxBySnd $ U.zip (U.fromList [0..nSamps]) vs
  where nSamps = U.length vs


------------------------------------------------------------------------------
-- | Find the argument whose second element is largest.
maxBySnd :: Ord b => (a,b) -- ^ First element being compared
                  -> (a,b) -- ^ Second element being compared
                  -> (a,b)
maxBySnd a@(_,v) b@(_,v') = if v > v' then a else b


------------------------------------------------------------------------------
-- | Find all channel amplitudes at the time when the greatest voltage occurred
--   around a spike.
spikeAmplitudes  :: TrodeSpike -- ^ The spike of interest
                 -> V.Vector Voltage
spikeAmplitudes s = V.map (U.! i) (spikeWaveforms s)
  where i = spikePeakIndex s


------------------------------------------------------------------------------
-- | Representation of spike features
data SpikeModel = SpikeModel { -- TODO: determine what exactly this means
                               -- | The time of the spike
                               mSpikeTime          :: ExperimentTime
                               -- | The channel amplitudes at the time of
                               --   greates voltage
                             , mSpikePeakAmp       :: U.Vector Voltage
                               -- | The time between the peak and trough of
                               --   the spike.
                             , mSpikepPeakToTroughT :: DiffTime
                               -- | The voltage difference between the peak and
                               --   the trough of the spike.
                             , mSpikepPeakToTroughV :: U.Vector Voltage
                             } deriving (Show)
-- TODO: Do I need the rest of the mwl params?  maxwd? maxh?
-- What about things like 'noise'?  Or 'deviation from the cluster'?


------------------------------------------------------------------------------
-- This should be part of arte, not tetrode-ephys?  It's about recording
-- But I need it to decode files...
-- | Trode acquisition parameters
data TrodeAcquisitionOpts = TrodeAcquisitionOpts { -- | Thresholds for spike
                                                   --   detection
                                                   spikeThresholds :: [Voltage]
                                                 } deriving (Eq, Show)


------------------------------------------------------------------------------
instance S.Serialize TrodeSpike where
  put TrodeSpike{..} = do
    S.put spikeTrodeName
    S.put spikeTime
    S.put spikeWaveforms
  get = do
    name <- S.get
    time <- S.get
    waveforms <- S.get
    return $ TrodeSpike name time waveforms

------------------------------------------------------------------------------
instance B.Binary TrodeSpike where
  put TrodeSpike{..} = do
    B.put spikeTrodeName
    B.put spikeTime
    B.put spikeWaveforms
  get = do
    name <- B.get
    time <- B.get
    waveforms <- B.get
    return $ TrodeSpike name time waveforms
