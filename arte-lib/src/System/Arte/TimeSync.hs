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

module System.Arte.TimeSync where

import Data.Time.Clock
import Options.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.Binary.Get
import qualified Network.Socket.ByteString as BS
import Control.Concurrent
import Network.Socket
import Data.Ratio

-- | The state of network time synchronization
data TimeSyncState = TimeSyncState {
    -- | The clock time when we last synced up
    localTimeAtLastSync :: !UTCTime
    -- | The network time when we last synced up.
    --   Since network time is just a monotonically increasing number, we
    --   represent it as an offset. Absolute magnitudes aren't important, we
    --   only care about differences between these.
  , networkTimeAtLastSync  :: !DiffTime
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
      let Done rest _ seconds = pushChunk (runGetIncremental getWord64le) buf
          Done _ _ nanoseconds = pushChunk (runGetIncremental getWord64le) rest
          s = toInteger seconds
          n = toInteger nanoseconds
          ntNow = fromRational ((s % 1) + (n % 1000000000))
      return $ TimeSyncState sysNow ntNow

    listenForTimePackets sock stateVar = forever $ do
      st <- getState sock
      atomically $ writeTVar stateVar st
