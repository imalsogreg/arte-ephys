{-# LANGUAGE RecordWildCards #-}

module System.Arte.TimeClient where

import Data.Time.Clock
import Data.Ephys
import Network
import Options.Applicative
import Control.Concurrent.STM

data TimeOptions = TimeOptions {
    timeServerHost :: HostName
  , timeServerPort :: String
 } deriving (Show)

data TimeClientState = TimeClientState {
    localSystemTimeAtLastSync :: UTCTime
  , experimentTimeAtLastSync  :: ExperimentTime
  }

timeOptions :: Parser TimeOptions
timeOptions = TimeOptions
              <$> strOption
              ( long "timeServerHost"
              <> help "IP address of the timestamp server")
              <*> strOption
              ( long "timeServerPort"
              <> help "Remote timestamp server port")

data TimeQuery = TimeQuery {
    timeTillExperimentTime   :: ExperimentTime -> IO Double
  , getCurrentExperimentTime :: IO ExperimentTime
    }

setupTimeQuery :: TimeOptions -> IO TimeQuery
setupTimeQuery TimeOptions{..} = do

  sock <- socket AF_INET Datagram defaultProtocol
  let saddr = SockAddrInet (myPort) timeServerHost

  stateVar <- newTVarIO Nothing

  forkIO $ listenForTimePackets sock stateVar

  let getTimeNow = do
        sysNow <- getCurrentTime
        (TimeClientState sysOld expOld) <- readTVarIO stateVar
  -- TO BE CONTINUED
