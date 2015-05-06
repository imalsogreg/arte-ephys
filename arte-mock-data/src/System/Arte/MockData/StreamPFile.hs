{-# LANGUAGE RecordWildCards #-}
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
import Types
import GHC.Word

import Data.Ephys.Position
import Data.Ephys.OldMWL.ParsePFile
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.ParseSpike
import Data.Ephys.OldMWL.Header
import Data.Ephys.OldMWL.Parse
import Data.Ephys.OldMWL.ParsePFile

streamP :: DataSourceOpts -> IO ()
streamP DataSourceOpts{..} = withSocketsDo $ do
 sock <- socket AF_INET Datagram defaultProtocol
 let saddr = SockAddrInet (fromIntegral myPort) iNADDR_ANY
 destAddr <- SockAddrInet
             (fromIntegral (destPort :: Word32))
             <$> inet_addr ipAddy
 bindSocket sock saddr
 f <- BSL.readFile fileName
 fi <- getFileInfo fileName
 case fi of
  Left e -> error $ "Error reading file " ++ fileName ++ " " ++ e
  Right fi' ->
    runEffect $ dropResult (produceMWLPos f) >->
    relativeTimeCat (\s -> _mwlPosTime s - expStartTime) >->
    runningPosition (166, 140) 156.6 0.5 pos0 >->
    (forever $ do
        pos <- await
        liftIO $ BS.sendAllTo sock (encode pos) destAddr
    )

pos0 :: Position
pos0 = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0
       ConfSure sZ sZ (-100 :: Double) (Location 0 0 0)
       where sZ = take 5 (repeat 0)
