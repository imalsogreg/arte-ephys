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
import Data.Maybe
import Options.Applicative

data Opts = Opts
            { server :: Bool
            , client :: Bool
            , host   :: String
            , port   :: String
            }

opts :: Parser Opts
opts = Opts
       <$> switch
       ( long "server" <> short 's' <> help "Run as server")
       <*> switch
       ( long "client" <> short 'c' <> help "Run as client")
       <*> strOption
       ( long "host" <> short 'h' <> help "Hostname")
       <*> strOption
       ( long "port" <> short 'p' <> help "Port number")
       
node :: String -> String -> Node
node h p = Node h (Host h h) (read p)

run :: Opts -> IO ()
run (Opts True False h p) = serve h p
run (Opts False True h p) = cli   h p
run _ = error "Must run as either server or client"

serve :: String -> String -> IO ()
serve h p = do
      q <- atomically $ newTQueue
      pub <- atomically $ DataPublisher q <$> newTVar []
      pubA <- async $ acceptSubscribers (node h p) pub
      forM_ [(1::Int)..] $ \n -> do
        print $ "Writing to queue " ++ show n
        atomically (writeTQueue q n)
        threadDelay 500000
      wait pubA

cli :: String -> String -> IO ()
cli h p = do
      withSubscription (node h p) $ \a ->
        case (a :: Either String Int) of
          Left e           -> print $ "Error: " ++ e
          Right x -> print $ show x

main :: IO ()
main = execParser opt >>= run
  where opt = info (helper <*> opts)
         (fullDesc <> progDesc "Test" <> header "Test")