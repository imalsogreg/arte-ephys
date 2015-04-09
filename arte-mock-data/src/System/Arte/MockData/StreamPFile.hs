module System.Arte.MockData.StreamPFile where

import Control.Applicative
import Control.Error
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
import Safe

import Data.Ephys.OldMWL.ParsePFile
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.ParseSpike
import Data.Ephys.OldMWL.Header
import Data.Ephys.OldMWL.Parse

{-
-- commented out to get other parts working
main :: IO ()
main = withSocketsDo $ do
  op <- validateInput <$> getArgs
  case op of
   Left e -> error e
   Right (Opts fileName destIPAddy myPort destPort expStartTime) -> do
     sock <- socket AF_INET Datagram defaultProtocol
     let saddr = SockAddrInet (fromIntegral myPort) iNADDR_ANY
     destAddr <- SockAddrInet
                 (fromIntegral (destPort :: Int))
                 <$> inet_addr destIPAddy
     bindSocket sock saddr
     f <- BSL.readFile fileName
     fi <- getFileInfo fileName
     case fi of
      Left e -> error $ "Error reading file " ++ fileName ++ " " ++ e
      Right fi' ->
        runEffect $ dropResult (produceMWLPos f) >->
        relativeTimeCat (\s -> _mwlPosTime s - expStartTime) >->
        (forever $ do
            pos <- await
            liftIO $ BS.sendAllTo sock (encode pos) destAddr
        )

data Opts = Opts {
    fileName  :: String
  , destIP    ::String
  , myPort    :: Int
  , destPort  :: Int
  , startTime :: Double
  } deriving (Show)

------------------------------------------------------------------------------
validateInput :: [String] -> Either String Opts
validateInput [fn, dIP, mP, dP, sT] =
  Opts fn dIP
  <$> note' "Bad self port"  (readMay mP)
  <*> note' "Bad dest port"  (readMay dP)
  <*> note' "Bad start time" (readMay sT)
  where note' s = note (unlines [s,usage])
validateInput _ = Left $ unlines ["Wrong number of arguments",usage]

usage :: String
usage = unlines ["Usage:"
                ,"streamPFile P_FILE MULTICAST_IP SOURCE_PORT DESTINATION_PORT START_TIME"
                ]
-}
