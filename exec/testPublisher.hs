{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Applicative
import System.Environment
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Lens
import Control.Monad
import qualified Data.HashMap.Strict as Hash
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Ord
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import Network
import Options.Applicative
import System.Posix
import System.IO

import System.Arte.DataPublisher
import System.Arte.Net
import System.Arte.CommandPort

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
  _ <- installHandler sigPIPE Ignore Nothing
  q <- atomically $ newTQueue
  t <- atomically $ newTVar 1
  let node2 = Node h (Host h h) ((read p)+portAdd)
  pub <- atomically $ DataPublisher q <$> newTVar []
  c   <- async $ acceptClients node2 (handleCmd t)
  pubA <- async $ acceptSubscribers (node h p) pub
  runA <- async $ runPublisher (pub :: DataPublisher Integer) :: IO (Async ())
  forM_ [(1000000::Integer)..] $ \n -> do
    v <- atomically $ do
         v <- readTVar t
         writeTQueue q  (v+n)
         return v
    threadDelay $ fromIntegral (v+n)
  wait pubA
  wait runA

portAdd = 1

------------------------------------------------------------------------------
handleCmd :: TVar Integer -> BSL.ByteString -> IO BSL.ByteString
handleCmd r req =
  case (req ^? key "command") of
    Just (String "setMult") ->
      case req ^? key "args" . _Value  . nth 0 . _String of
        Just n  -> atomically $ writeTVar r (read . T.unpack $ n) >>
                   return "{\"response\":\"ok\"}"
        Nothing -> return "{\"response\":\"badRequest: no args [0]\"}"
    _ -> return "{\"response\":\"badRequest: no command\"}"

cli :: String -> String -> IO ()
cli h p = do
  listener <- async $ withSubscription (node h p) $ \a ->
    putStrLn "Running" >>
    case (a :: Either String Integer) of
      Left e           -> print ("Error: " ++ e) >>
                          return False
      Right x -> print (show x) >> return True
  let node = (Node h (Host h h) ((read p)+portAdd))
  withCommandPort node $ \hCmd -> forever $ do
    l <- (T.words . T.pack) <$> getLine
    BSL.putStrLn . encode . makeCommand $ l
    BS.hPutStrLn hCmd . BSL.toStrict . encode . makeCommand $ l

makeCommand :: [T.Text] -> Object
makeCommand xs = Hash.fromList [("command",String $ head xs)
                               ,("args",Array args)]
  where args = V.fromList $ map String (tail xs)

main :: IO ()
main = execParser opt >>= run
  where opt = info (helper <*> opts)
         (fullDesc <> progDesc "Test" <> header "Test")


-- a little test
cli2 :: String -> String -> IO ()
cli2 h p = do
  hnd <- connectTo h (PortNumber . fromIntegral . read $ p)
  putStrLn "Client got a connection!"
  forever $ do
    putStrLn "hGetChar"
    c <- hGetChar hnd
    putStrLn "putChar"
    putStrLn . show . fromEnum $ c

