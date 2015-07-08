{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (forever)
import Control.Concurrent.Async
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import Data.Traversable
import qualified Data.Vector as V
import Options.Applicative
import GHC.Word
import Control.Concurrent.STM
import System.Arte.Decode.Types
import Data.Ephys.EphysDefs
import Network.Socket
import qualified Data.Map as Map
import Pipes
import Pipes.RealTime
import Data.Ephys.Position
import Data.Ephys.TrackPosition
import System.Arte.Net (udpSocketProducer)
import System.Arte.Decode.Config (pos0, field0)
import System.Arte.Decode.Algorithm (normalize)

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

  print opts

  (estSock,posSock) <- setupSockets opts

  posThread <- async . runEffect $ udpSocketProducer 9000 posSock >->
    (forever $ await >>= \p -> liftIO $
                               atomically $ writeTVar (ratPos masterState) p)

  slavesThread <- async . runEffect $ udpSocketProducer 9090 estSock >->
    (forever $ await >>= \est -> liftIO (acceptEstimate masterState est))

  combineThread <- async . runEffect $ forever (yield ())
    >-> steadyCat 100
    >-> forever (await >> liftIO (updateMasterEstimate masterState))

  runMasterGui undefined


data MasterState = MasterState {
    tetrodeEstimate :: TVar (Map.Map TrodeName Field)
  , ratPos          :: TVar Position
  , masterEstimate  :: TVar Field
  }

type Estimate = Int

acceptEstimate :: MasterState -> Estimate -> IO ()
acceptEstimate = undefined

-- TODO: ? Profile and decide: switch to Rational (infinite precision)
updateMasterEstimate :: MasterState -> IO ()
updateMasterEstimate MasterState{..} = do
  fields <- Map.elems <$> readTVarIO tetrodeEstimate
  let fieldLogs = V.map log <$> fields
      logSums   = V.map (L.foldl' (+) 0) (sequenceA fieldLogs)
      fieldPDF  = normalize (V.map exp logSums)
  atomically . writeTVar masterEstimate $ fieldPDF


setupSockets :: Opts -> IO (Socket,Socket)
setupSockets Opts{..} = do
  estSock <- socket AF_INET Datagram defaultProtocol
  bind estSock $ SockAddrInet (fromIntegral estPort) iNADDR_ANY
  posSock <- socket AF_INET Datagram defaultProtocol
  bind posSock $ SockAddrInet (fromIntegral posPort) iNADDR_ANY
  return (estSock,posSock)

newMasterState :: IO MasterState
newMasterState = do
  ests <- newTVarIO Map.empty
  p    <- newTVarIO pos0
  est0 <- newTVarIO field0
  return $ MasterState ests p est0
