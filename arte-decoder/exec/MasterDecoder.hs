{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (forever)
import Control.Concurrent.Async
import Control.Monad.IO.Class (liftIO)
import Data.Traversable
import Options.Applicative
import GHC.Word
import Control.Concurrent.STM
import System.Arte.Decode.Types
import Data.Ephys.EphysDefs
import Network.Socket
import qualified Data.Map as Map
import Pipes
import Data.Ephys.Position
import Data.Ephys.TrackPosition
import System.Arte.Net (udpSocketProducer)
import System.Arte.Decode.Config (pos0, field0)

data Opts = Opts {
    estPort :: Word16
  , posPort :: Word16
  , rTau    :: Double
  } deriving (Eq, Show)

optStruct :: Parser Opts
optStruct = Opts
       <$> option auto
       (long "estPort"
        <> short 'e'
        <> help "Port to listen for position estimates")
       <*> option auto
       (long "posPort"
        <> short 'p'
        <> help "Port to listen for rat position")
       <*> option auto
       (long "rTau"
       <> short 'r'
       <> help "Decoding interval rTau (seconds)")

masterOpts = info (helper <*> optStruct)
             (fullDesc
             <> progDesc "Decoder matser"
             <> header "master-decoder")

------------------------------------------------------------------------------
main :: IO ()
main = do
  masterState <- newMasterState
  opts <- execParser masterOpts
  (estSock,posSock) <- setupSockets opts
  async . runEffect $ udpSocketProducer 9000 posSock >->
    (forever $ await >>= \p -> liftIO $
                               atomically $ writeTVar (ratPos masterState) p)
  print opts

data MasterState = MasterState {
    tetrodeEstimate :: Map.Map TrodeName (TVar (Field))
  , ratPos          :: TVar Position
  , masterEstimate  :: TVar Field
  }

setupSockets :: Opts -> IO (Socket,Socket)
setupSockets Opts{..} = do
  estSock <- socket AF_INET Datagram defaultProtocol
  bind estSock $ SockAddrInet (PortNum estPort) iNADDR_ANY
  posSock <- socket AF_INET Datagram defaultProtocol
  bind posSock $ SockAddrInet (PortNum posPort) iNADDR_ANY
  return (estSock,posSock)

newMasterState :: IO MasterState
newMasterState = MasterState <$> return Map.empty <*> newTVarIO pos0 <*> newTVarIO field0

------------------------------------------------------------------------------
showMasterState :: MasterState -> IO String
showMasterState MasterState{..} =
  (\tEs p mE ->
     unlines [ "MasterState {", show tEs, show p, show mE, "}"])
  <$> traverse readTVarIO tetrodeEstimate
  <*> readTVarIO ratPos
  <*> readTVarIO masterEstimate

