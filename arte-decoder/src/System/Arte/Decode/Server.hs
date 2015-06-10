module System.Arte.Decode.Server where

import Data.Monoid
import GHC.Word
import Options.Applicative
import Network
import Network.Socket.ByteString
import System.Arte.Decode.Types
import Data.Ephys.TrackPosition


data ServerOpts = ServerOpts {
    ipAddy   :: String
  , myPort   :: Word32
  , destPort :: Word32
  } deriving (Show)

serverOpts :: Parser ServerOpts
serverOpts = ServerOpts
             <$> strOption
             ( long "ip"
             <> help "Server destination ip address")
             <*> option auto
             ( long "localPort"
             <> help "Local sending port number")
             <*> option auto
             ( long "destPort"
             <> help "Receiver's port at destination")
