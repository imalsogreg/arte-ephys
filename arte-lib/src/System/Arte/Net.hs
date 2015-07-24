{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveGeneric #-}

module System.Arte.Net where

import Control.Monad.IO.Class (liftIO)
import Pipes
import Data.Serialize
import qualified Network.Socket.ByteString as BS
import Network
import System.IO

udpSocketProducer :: (Serialize t) => Int -> Socket -> Producer t IO ()
udpSocketProducer buffSize s = forever $ do
  (buf, _) <- liftIO $ BS.recvFrom s buffSize
  case decode buf of
    Left e -> liftIO $ hPutStrLn stderr ("Error parsing packet: " ++ e)
    Right t -> yield t
