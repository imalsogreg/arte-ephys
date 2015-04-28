----------------------------------------------------------------------
-- |
-- Module      : Simple
-- Copyright   : (c) Greg Hale
-- License     : BSD3
--
-- Maintainer  : Greg Hale <imalsogreg@gmail.com>
-- Stability   : unstable
-- Portability :
--
-- A simple demo program that acts like any other slave process
-- For experimenting with zmq, gtk, etc
--
----------------------------------------------------------------------

module System.Arte.Simple where

{-# LANGUAGE OverloadedStringss #-}

import System.Arte.Net
import System.Arte.NetMessage

import Prelude as P
import Options.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import System.Environment
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.Async
import qualified Data.Serialize as S

{-
main :: IO ()
main = do
  masterNode' <- getAppNode "master"  Nothing
  myNode'     <- getAppNode "spikesA" Nothing
  case (masterNode',myNode') of
    (Right masterN, Right myN) -> withMaster masterN $ \(toMaster,fromMaster) -> do
      a  <- async $ sendMessages toMaster
      receiveMessages fromMaster
      wait a

receiveMessages :: TQueue ArteMessage -> IO ()
receiveMessages queue = forever $ do
  m' <- atomically $ readTQueue queue
  putStrLn $ "Got response "  ++ show m'

sendMessages :: TQueue ArteMessage -> IO ()
sendMessages queue = loop
  where
    loop = do
      putStrLn $ "Enter a command (ping or quit)"
      line <- getLine
      message <- case line of
            "ping"   -> return NetPing
            "quit"   -> return ForceQuit
            "hangup" -> return ServerHangup
      atomically $ writeTQueue queue (ArteMessage 0 "" Nothing (Request message))
      case message of
        ForceQuit -> return ()
        _         -> loop
-}
