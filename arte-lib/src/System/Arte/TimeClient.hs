{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Data.Ratio

data TimeOptions = TimeOptions {
    timeClientPort :: String
 } deriving (Show)

newtype Seconds = Seconds Int64 deriving ( Show
                                         , Num
                                         , Integral
                                         , Ord
                                         , Eq
                                         , Enum
                                         , Real
                                         )
newtype Nanoseconds = Nanoseconds Int64 deriving ( Show
                                                 , Num
                                                 , Integral
                                                 , Ord
                                                 , Eq
                                                 , Enum
                                                 , Real
                                                 )

data NetworkTime = NetworkTime {
    seconds :: Seconds
  , nanoseconds :: Nanoseconds
  } deriving (Show)

diffNetworkTime :: NetworkTime -> NetworkTime -> NetworkTime
diffNetworkTime a b = NetworkTime s ns
  where
    ans = nanoseconds a
    bns = nanoseconds b
    (ns, carry) = if ans >= bns
      then (ans - bns, False)
      else (ans + 1000000000 - bns, True)
    as = seconds a
    bs = seconds b
    s = (if carry then as - 1 else as) - bs

data TimeClientState = TimeClientState {
    localSystemTimeAtLastSync :: UTCTime
  , networkTimeAtLastSync  :: NetworkTime
  }

timeOptions :: Parser TimeOptions
timeOptions = TimeOptions
              <$> strOption
              ( long "timeClientPort"
              <> help "Timestamp client port")

networkTimeToSysTime :: TimeClientState -> NetworkTime -> UTCTime
networkTimeToSysTime TimeClientState{..} nt =
    addUTCTime diffTime localSystemTimeAtLastSync
  where
    ntDiff = diffNetworkTime nt networkTimeAtLastSync
    iseconds = toInteger (seconds ntDiff)
    inanoseconds = toInteger (nanoseconds ntDiff)
    diffTime = fromRational ((iseconds % 1) + (inanoseconds % 1000000000))

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
      let ntm = NetworkTime (fromIntegral seconds) (fromIntegral nanoseconds)
      return $ TimeClientState sysNow ntm

    listenForTimePackets sock stateVar = forever $ do
      st <- getState sock
      atomically $ writeTVar stateVar st
