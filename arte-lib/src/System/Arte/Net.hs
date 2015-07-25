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
import Pipes
import Data.Serialize
import qualified Network.Socket.ByteString as BS
import Network
import System.IO

-- | Produce a stream of ts by reading them from a socket forever
udpSocketProducer :: (Serialize t) => Int -- ^ Buffer size for packets
                                   -> Socket -- ^ Socket to listen on
                                   -> Producer t IO ()
udpSocketProducer buffSize s = forever $ do
  (buf, _) <- liftIO $ BS.recvFrom s buffSize
  case decode buf of
    Left e -> liftIO $ hPutStrLn stderr ("Error parsing packet: " ++ e)
    Right t -> yield t
