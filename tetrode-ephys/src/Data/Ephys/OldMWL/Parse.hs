{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}

module Data.Ephys.OldMWL.Parse where

import Data.Ephys.OldMWL.Header
import Control.Monad (forM_, replicateM, forever)
--import Control.Monad.Trans.State
import Data.Maybe (listToMaybe)
import qualified Data.ByteString.Lazy as BSL hiding (map, any, zipWith)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Int
import Control.Lens (view)
import Pipes
import qualified Pipes.Prelude as PP
import Data.Binary
import Data.Binary.Put
import qualified Pipes.Binary as PBinary hiding (Get)
import Pipes.Binary (decoded, decodeGetL,decodeGet)
import Data.Binary.Get (getWord32le, getWord16le)
import qualified Pipes.ByteString as PBS 
import qualified Data.Text as T
--import Data.Packed.Matrix
import qualified Data.List as List
-- import Data.Packed.Vector (Vector, toList)

import Pipes.Parse

import qualified Data.Ephys.Spike as Arte
import Data.Ephys.OldMWL.FileInfo

{-
-- TODO: This definition is moving over to Data.Ephys.OldMWL.ParseSpike
data MWLSpike = MWLSpike { mwlSpikeTime      :: Double
                         , mwlSpikeWaveforms :: [U.Vector Double] -- Double is
                                                                  -- MWL units
                                                                  -- though
                         } deriving (Eq, Show)
-}

decodeTime :: Word32 -> Double
decodeTime = (/ 10000) . fromIntegral

encodeTime :: Double -> Word32
encodeTime = floor . (* 10000)


-- 'cast'int word to int, right?
word16ToInt16 :: Word16 -> Int16
word16ToInt16 x = fromIntegral x - ( (fromIntegral (maxBound :: Word16)) `div` 2) - 1

word32ToInt32 :: Word32 -> Int32
word32ToInt32 x = fromIntegral x - ( (fromIntegral (maxBound :: Word32)) `div` 2) - 1

-- TODO Is this right?
int16toWord16 :: Int16 -> Word16
int16toWord16 x = fromIntegral x - fromIntegral (maxBound :: Word16) - 1

int32toWord32 :: Int32 -> Word32
int32toWord32 x = fromIntegral x - fromIntegral (maxBound :: Word32) - 1


{-
-- Test this out. Didn't work!  Turned good import bad.
word16ToInt16 = fromIntegral
word32ToInt32 = fromIntegral
int16toWord16 = fromIntegral
int32toWord32 = fromIntegral
-}

{-
parseSpike :: FileInfo -> Get MWLSpike
parseSpike fi@FileInfo{..}
  | True = -- okSpikeFile fi = --FIXME
    -- tsType unused because we're assuming tsType -> double.  Fix this by figuring out the
    -- MWL int to type code
    --let gains = fileGains fi
    -- TODO REMOVE PARTIAL FUNCTION
    let Just (_,_,totalSampsPerSpike) = listToMaybe $ filter (\(n,_,_) -> n == "waveform") hRecordDescr
    in do
      ts <- getWord32le :: Get Word32
      -- grab one sample at a time (word16) from the stream: nchans * nsampsperchan
      vs <- replicateM (fromIntegral totalSampsPerSpike) getWord16le
      let fI = fromIntegral
          vsInt = map (fI . word16ToInt16) vs
          -- Make a matrix of the sample vector (>< is from Data.Packed.Matrix)
          vsMat  = fI (totalSampsPerSpike `div` hNTrodeChans) >< fI hNTrodeChans $ vsInt
          -- Transpose it into a list (toColumns from Data.Packed.Matrix)
          vsVecs = toColumns vsMat :: [Data.Packed.Vector.Vector Double]
          -- From Matrix's Vector to the usual Vector
          vsUVecs = map hMatrixVecToUnboxedVec vsVecs
      return $ MWLSpike (decodeTime ts) vsUVecs
  | otherwise    = error "Failed okFileInfo test"
-}

getMany :: Monad m => Get a -> Producer PBS.ByteString m r -> Producer a m PBinary.DecodingError
getMany getA = go
  where go p = do
          (!x, !p') <- lift $ runStateT (decodeGet getA) p
          case x of
            Left err -> return err
            Right !a -> do
              yield a
              go p'


-- TODO: This is misplaced.  Need something like "general utils."
dropResult :: (Monad m) => Proxy a' a b' b m r -> Proxy a' a b' b m ()
dropResult p = p >>= \_ -> return ()

