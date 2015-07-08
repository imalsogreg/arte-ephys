{-|
Module      : System.Arte.NetworkTime
Description : Types for arte network time
Copyright   : (c) Greg Hale, 2015
                  Shea Levy, 2015
License     : BSD3
Maintainer  : imalsogreg@gmail.com
Stability   : experimental
Portability : GHC, Linux
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Arte.NetworkTime ( NetworkTime -- Don't export constructors!
                               , diffNetworkTime
                               , addNetworkTime
                               , networkTimeEpoch
                               ) where

import Data.Time.Clock
import Data.Ratio
import Data.Serialize
import Data.Int
import Data.Word

newtype Seconds = Seconds Int64 deriving ( Integral, Real, Enum, Num, Ord, Eq )

newtype Nanoseconds = Nanoseconds Word32 deriving ( Integral
                                                  , Real
                                                  , Enum
                                                  , Num
                                                  , Ord
                                                  , Eq
                                                  )

-- | Time synchronized over the network
--
--   Only differences between times have real semantics.
--   Resolution in nanoseconds.
data NetworkTime = NetworkTime !Seconds !Nanoseconds

-- | 'diffNetworkTime' a b = a - b
diffNetworkTime :: NetworkTime -> NetworkTime -> DiffTime
diffNetworkTime (NetworkTime aS aN) (NetworkTime bS bN) =
    fromRational $ (diffS % 1) + (diffN % 1000000000)
  where
    diffS = fromIntegral $ aS - bS
    diffN = fromIntegral $ aN - bN

-- | 'addNetworkTime' a b = a + b
addNetworkTime :: DiffTime -> NetworkTime -> NetworkTime
addNetworkTime dt (NetworkTime s ns) = NetworkTime sumS sumNs
  where
    dtS = floor dt
    dtfrac = dt - (fromIntegral s)
    dtNs = round (dtfrac * 1000000000)
    (sumNs, carry) = let rawSum = dtNs + ns in if rawSum >= 1000000000
      then (rawSum - 1000000000, True)
      else (rawSum, False)
    sumS = s + dtS + (if carry then 1 else 0)

-- | An arbitrary fixed base time.
--   Since 'networkTimeEpoch' is a fixed constant, multiple processes can
--   determine pre-fixed offsets from it and compare them to the network time
--   pulled from the wire to trigger events at the same time.
networkTimeEpoch :: NetworkTime
networkTimeEpoch = NetworkTime (Seconds 0) (Nanoseconds 0)

instance Serialize NetworkTime where
  put (NetworkTime s ns) = do
      putWord64le $ fromIntegral s
      putWord64le $ fromIntegral ns

  get = do
    seconds <- fromIntegral <$> getWord64le
    nanoseconds <- fromIntegral <$> getWord64le
    return $ NetworkTime seconds nanoseconds
