{-|
Module      : System.Arte.Net
Description : Miscellaneous networking utilities
Copyright   : (c) Greg Hale, 2015
                  Shea Levy, 2015
License     : BSD3
Maintainer  : imalsogreg@gmail.com
Stability   : experimental
Portability : GHC, Linux
-}
module System.Arte.Net where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Pipes
import Data.Serialize
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Network.Socket.ByteString as BS
import Network
import System.IO


-- | Produce a stream of Serializable ts by reading them from a socket forever
udpSocketProducer :: (Serialize t) => Int -- ^ Buffer size for packets
                                   -> Socket -- ^ Socket to listen on
                                   -> Producer t IO ()
udpSocketProducer = udpSocketProducerWith decode



-- | Produce a stream of FromJSON ts by reading them from a socket forever
udpJsonProducer :: (A.FromJSON t) => Int -- ^ Buffer size for packets
                                  -> Socket -- ^ Socket to listen on
                                  -> Producer t IO ()
udpJsonProducer = udpSocketProducerWith (A.eitherDecode . BL.fromStrict)


-- | Produce o stream of ts by reading from a socket forever,
--   with a custom conversion function
udpSocketProducerWith :: (BS.ByteString -> Either String t)
                      -- ^ Conversion from bytes to t
                      -> Int                  -- ^ Maximum buffer length
                      -> Socket               -- ^ Incoming data socket
                      -> Producer t IO ()
udpSocketProducerWith f buffSize s = forever $ do
  (buf, _) <- liftIO $ BS.recvFrom s buffSize
  case f buf of
    Left e -> liftIO $ hPutStrLn stderr ("Error parsing packet: " ++ e)
    Right t -> yield t
