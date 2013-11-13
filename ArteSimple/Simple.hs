----------------------------------------------------------------------
-- |
-- Module      : Simple
-- Copyright   : (c) Greg Hale
-- License     : GPL-3
-- 
-- Maintainer  : Greg Hale <imalsogreg@gmail.com>
-- Stability   : unstable 
-- Portability :
--
-- A simple demo program that acts like any other slave process
-- For experimenting with zmq, gtk, etc
-- 
----------------------------------------------------------------------

module Main where

{-# LANGUAGE OverloadedStringss #-}

import Arte.Common
import Arte.Common.Net
import Ante.Common.NetMessage

import Prelude as P
import Data.ByteString
import Data.ByteString.Char8 as C
import System.Environment
import System.ZMQ
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async

main :: IO ()
main = do
  masterNode' <- getAppNode "master"  Nothing
  myNode'     <- getAppNode "spikesA" Nothing
  case (masterNode',myNode') of
    (masterN,meN) -> withMaster masterN $ \(toMaster,fromMaster) -> do
      a  <- async $ sendMessages toMaster
      receiveMessages fromMaster
      wait a

receiveMessages :: ZMQ.Socket -> IO ()
receiveMessages sock = forever $ do
  m' <- receive sock []
  case m' of
    Left e  -> putStrLn $ "Bad response: " ++ e
    Right m -> putStrLn $ "Got response "  ++ show m

sendMessages :: Handle -> IO ()
sendMessages h = hSetBuffering NoBuffering >> loop
  where
    loop = do
      line <- getLine
      message <- case line of
            "ping" -> return NetPing
            "quit" -> return ForceQuit
      sendWithSize h message
      case message of
        ForceQuit -> return ()
        _         -> loop
        
    