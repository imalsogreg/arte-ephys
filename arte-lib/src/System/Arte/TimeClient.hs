{-# LANGUAGE RecordWildCards #-}

module System.Arte.TimeClient where

import Data.Time.Clock
import Network
import Options.Applicative
import Control.Concurrent.STM
import Data.Int
import Control.Monad
import Data.Binary.Get
import qualified Network.Socket.ByteString as BS
import Control.Concurrent
import Network.Socket

data TimeOptions = TimeOptions {
    timeClientPort :: String
 } deriving (Show)

-- TODO: Sync with Data.Ephys.ExperimentTime
data ExperimentTime = ExperimentTime {
    seconds :: Int64
  , nanoseconds :: Int64
  }

data TimeClientState = TimeClientState {
    localSystemTimeAtLastSync :: UTCTime
  , experimentTimeAtLastSync  :: ExperimentTime
  }

timeOptions :: Parser TimeOptions
timeOptions = TimeOptions
              <$> strOption
              ( long "timeClientPort"
              <> help "Timestamp client port")

setupTimeQuery :: TimeOptions -> IO (TVar TimeClientState, ThreadId)
setupTimeQuery TimeOptions{..} = do

    sock <- socket AF_INET Datagram defaultProtocol
    setSocketOption sock ReuseAddr 1
    let saddr = SockAddrInet (fromInteger $ read timeClientPort) iNADDR_ANY
    bind sock saddr

    st <- getState sock
    stateVar <- newTVarIO $ st

    tid <- forkIO $ listenForTimePackets sock stateVar
    return (stateVar, tid)
  where
    getState sock = do
      (buf, _) <- BS.recvFrom sock 16
      sysNow <- getCurrentTime
      let Done rest _ seconds = pushChunk (runGetIncremental getWord64le) buf
      let Done _ _ nanoseconds = pushChunk (runGetIncremental getWord64le) rest
      let etm = ExperimentTime (fromIntegral seconds) (fromIntegral nanoseconds)
      return $ TimeClientState sysNow etm

    listenForTimePackets sock stateVar = forever $ do
      st <- getState sock
      atomically $ writeTVar stateVar st
