{-|
Module      : System.Arte.TimeSync
Description : Facilities for synchronizing with arte network time
Copyright   : (c) Greg Hale, 2015
                  Shea Levy, 2015
License     : BSD3
Maintainer  : imalsogreg@gmail.com
Stability   : experimental
Portability : GHC, Linux
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Arte.TimeSync ( NetworkTime -- Constructors excluded on purpose
                            , diffNetworkTime
                            , addNetworkTime
                            , networkTimeEpoch
                            , TimeSyncState ( .. )
                            , TimeSyncOptions ( .. )
                            , timeSyncOptions
                            , setupTimeSync
                            ) where

import Data.Time.Clock
import Options.Applicative
import Control.Concurrent.STM
import Control.Monad
import qualified Network.Socket.ByteString as BS
import Control.Concurrent
import Network.Socket
import Data.Ratio
import Data.Serialize
import Data.Int
import Data.Word
import System.IO

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
    (sumNs, carry) = let sum = dtNs + ns in if sum >= 1000000000
      then (sum - 1000000000, True)
      else (sum, False)
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

-- | The state of network time synchronization
data TimeSyncState = TimeSyncState {
    -- | The clock time when we last synced up
    localTimeAtLastSync :: !UTCTime
    -- | The network time when we last synced up.
  , networkTimeAtLastSync  :: !NetworkTime
  }

-- | Configuration to set up network time synchronization
data TimeSyncOptions = TimeSyncOptions {
    -- | The port to listen on for time updates
    timeSyncPort :: !String
 } deriving (Show)

-- | Option parser to create a 'TimeSyncOptions'
timeSyncOptions :: Parser TimeSyncOptions
timeSyncOptions = TimeSyncOptions
              <$> strOption
              ( long "timeSyncPort"
              <> help "Network time synchronization port")

-- | Start time synchronization in a background thread. Blocks until the first
-- network time update is received.
setupTimeSync :: TimeSyncOptions -- ^ Configure synchronization
              -> IO (TVar TimeSyncState, ThreadId) -- ^ State that is updated every sync, and the ID of the spawned thread.
setupTimeSync TimeSyncOptions{..} = do

    sock <- socket AF_INET Datagram defaultProtocol
    setSocketOption sock ReuseAddr 1
    let saddr = SockAddrInet (fromInteger $ read timeSyncPort) iNADDR_ANY
    bind sock saddr

    st <- getState sock
    stateVar <- newTVarIO st

    tid <- forkIO $ listenForTimePackets sock stateVar
    return (stateVar, tid)
  where
    getState sock = do
      (buf, _) <- BS.recvFrom sock 16
      sysNow <- getCurrentTime
      case decode buf of
        Left e -> error ("Error parsing time packet: " ++ e)
        Right ntNow -> return $ TimeSyncState sysNow ntNow

    listenForTimePackets sock stateVar = forever $ do
      st <- getState sock
      atomically $ writeTVar stateVar st
