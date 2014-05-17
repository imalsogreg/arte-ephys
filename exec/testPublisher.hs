module Main where

import System.Arte.DataPublisher
import System.Arte.Net

import Control.Applicative
import System.Environment
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad

h :: Host
h = Host "ThisHost" "10.121.43.167"

node :: String -> Node
node s = Node s h 8001

main :: IO ()
main = do
  
  args <- getArgs
  case args of
    ["server"] -> do
      q <- atomically $ newTQueue
      pub <- atomically $ DataPublisher q <$> newTVar []
      pubA <- async $ acceptSubscribers (node "MeServer") pub
      forM_ [(1::Int)..] $ \n -> do
        print $ "Writing to queue " ++ show n
        atomically (writeTQueue q n)
        threadDelay 500000
      wait pubA
    ["client"] -> do
      withSubscription (node "ServerNode") $ \a ->
        case (a :: Either String Int) of
          Left e           -> print $ "Error: " ++ e
          Right x -> print $ show x
    _ -> error "Pass either server or client argument"