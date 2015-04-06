module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Network
import Data.Serialize
import Network.Socket
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

main :: IO ()
main = withSocketsDo $ do
  [fn,destIPAddy,myPort,destPort,startT] <- getArgs
  sock <- socket AF_INET Datagram defaultProtocol
  let trodeName = read . Text.unpack $ mwlTrodeNameFromPath fn :: Int
  let expStartTime = read startT
  let saddr = SockAddrInet (fromIntegral (read myPort :: Int)) iNADDR_ANY
  destAddr <- SockAddrInet (fromIntegral (read destPort :: Int)) <$> (inet_addr destIPAddy)
  bindSocket sock saddr
  f <- BSL.readFile fn
  fi <- getFileInfo fn
  case fi of
   Left e -> error $ "Error reading file " ++ fn ++ " " ++ e
   Right fi' ->
     runEffect $ dropResult (produceTrodeSpikes trodeName fi' f) >->
                 relativeTimeCat (\s -> spikeTime s - expStartTime) >->
      (forever $ do
       spike <- await
       liftIO $ BS.sendAllTo sock (encode spike) destAddr)
