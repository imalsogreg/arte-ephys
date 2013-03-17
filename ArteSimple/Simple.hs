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

import Prelude as P
import Data.ByteString
import Data.ByteString.Char8 as C
import System.Environment
import System.ZMQ
import Control.Monad
import ZmqUtils

main :: IO ()
main = do
  args <- getArgs
  if (P.length args == 2) 
    then do 
        [ip,port] <- (liftM $ P.take 2) getArgs
        runServer $ zmqStr Tcp ip port
    else helpMessage

respOfReq :: ByteString -> ByteString
respOfReq s = C.pack "Response for :" `append` s

handlePackets :: Socket Rep -> IO ()
handlePackets sock = do
  reqStr <- receive sock []
  let resp = respOfReq reqStr
  send sock resp []
  P.putStrLn $ "Got [" ++ C.unpack reqStr ++ 
    "] and sent [" ++ C.unpack resp ++ "]"
  if reqStr == C.pack "Quit" 
    then return ()
    else handlePackets sock
  

runServer :: String -> IO ()
runServer s = withContext 1 $ \context -> do
  withSocket context Rep $ \sock -> do
    bind sock s 
    handlePackets sock

helpMessage :: IO ()
helpMessage = do
  n <- getProgName
  P.putStrLn (helpStr n) where
  helpStr n = "Call " ++ n ++ " with two arguments, " ++
            "ip address and port number for this server."