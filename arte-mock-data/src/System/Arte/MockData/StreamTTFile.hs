{-# LANGUAGE RecordWildCards #-}

module System.Arte.MockData.StreamTTFile where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import GHC.Word
import Data.Traversable (traverse)
import Network
import Data.Serialize
import qualified Data.Vector.Unboxed as U
import Network.Socket
import qualified Data.ByteString as BS
import qualified Network.Socket.ByteString as BS
import System.Environment
import Pipes
import Pipes.RealTime
import qualified Data.Text as Text

import Data.Ephys.Spike
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.ParseSpike
import Data.Ephys.OldMWL.Header
import Data.Ephys.OldMWL.Parse
import Types

-- TODO Are we accidentally naming all tetrodes '0'??
streamTT :: DataSourceOpts -> IO ()
streamTT DataSourceOpts{..} = withSocketsDo $ do
  sock <- socket AF_INET Datagram defaultProtocol
  let trodeName = read . Text.unpack $ mwlTrodeNameFromPath fileName :: Int
  let saddr     = SockAddrInet (fromIntegral myPort) iNADDR_ANY
  destAddr <- SockAddrInet (fromIntegral destPort) <$> (inet_addr ipAddy)
  bindSocket sock saddr

  case outputFormat of
        ArteNew -> runEffect $ dropResult (produceTrodeSpikesFromFile fileName trodeName)
                   >-> relativeTimeCat (\s -> spikeTime s - expStartTime)
                   >-> (forever $ do
                           spike <- await
                           liftIO $ BS.sendAllTo sock (encode spike) destAddr)
        ArteOld -> runEffect $ dropResult (produceMWLSpikesFromFile fileName)
                   >-> relativeTimeCat (\s -> mwlSpikeTime s - expStartTime)
                   >-> (forever $ do
                           mwlSpike <- await
                           liftIO $ BS.sendAllTo sock (spikeBits 0 mwlSpike) destAddr)

spikeBits :: Int -> MWLSpike -> BS.ByteString
spikeBits trodeName s =                     -- while 'old arte' takes some real work
  let nChans  = Prelude.length (mwlSpikeWaveforms s)
      nSamps  = sum (map U.length (mwlSpikeWaveforms s))
      sPerCh  = nSamps `div` nChans
      nSpaces = mAX_FILTERED_BUFFER_TOTAL_SAMPLE_COUNT
      samps   = U.concat (mwlSpikeWaveforms s)
  in
   runPut $ do
     put (65 :: Word8)                              -- the old arte type code for a spike
     replicateM_ 3 (put (0 :: Word8))                 -- Use up the next three bytes
     putWord32be (floor $ 10000 * mwlSpikeTime s)   -- TS to 10kHz clock cycles
     putWord16be (fromIntegral trodeName)           -- 16 bit trode name
     putWord16be (fromIntegral nChans)              -- 16 bit n chans
     putWord16be (fromIntegral sPerCh)              -- 16 bin n samps per chan
     mapM_ putMicroVolts (U.toList samps)           -- put the samps, change units to microvolts
     replicateM_ (nSpaces - nSamps) (putWord16be 0) -- fill in the gaps with 0's
     replicateM_ mAX_N_CHANS (putWord16be 1000000)  -- Put 1e6 as the gain in every gain slot
     replicateM_ mAX_N_CHANS (putMicroVolts 65)     -- put 65 microvolts in every threshold slot
     putWord16be 10                                 -- Assume sample 10 is the triggering index TODO fix
     putWord32be 0                                  -- Use '0' as the sequence number. We don't have seq info

putMicroVolts :: Double -> Put
putMicroVolts s = putWord16be (fromIntegral (floor (s * 1000000)) :: Word16)

mAX_N_CHANS :: Int   -- MAX_FILTERED_BUFFER_N_CHANS from old arte-backend global-defs.h
mAX_N_CHANS = 32
mAX_FILTERED_BUFFER_LEN     :: Int
mAX_FILTERED_BUFFER_LEN     = 320
mAX_FILTERED_BUFFER_TOTAL_SAMPLE_COUNT :: Int
mAX_FILTERED_BUFFER_TOTAL_SAMPLE_COUNT =
  mAX_N_CHANS * mAX_FILTERED_BUFFER_LEN
