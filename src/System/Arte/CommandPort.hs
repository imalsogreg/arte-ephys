module System.Arte.CommandPort where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Concurrent
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Data.Text.Encoding
import Network
import System.IO
import Text.Printf

import System.Arte.Net

------------------------------------------------------------------------------
acceptClients :: Node -> (BSL.ByteString -> IO BSL.ByteString) -> IO ()
acceptClients rep talker = do
  sock <- listenOn (PortNumber $ fromIntegral (rep^.nodeServerPort))
  forever $ do
    (h,host,port) <- accept sock
    _ <- printf "%s accepted command connection from %s\n" host (show port)
    hSetBuffering h NoBuffering
    forkFinally (putStrLn "Talking!" >> talk h talker) (\_ -> hClose h)

withCommandPort :: Node -> (Handle -> IO a) -> IO a
withCommandPort server action = do
  h <- connectTo (server ^. nodeHost . hostIp)
    (PortNumber $ fromIntegral (server ^. nodeServerPort))
  putStrLn "Got a client connection to command port."
  action h

-- internal
talk :: Handle -> (BSL.ByteString -> IO BSL.ByteString) -> IO ()
talk h talker = do
  l <- BSL.fromStrict <$> BS.hGetLine h
  putStrLn $ "Got message: " ++ BSL.unpack l
  resp <- talker l
  catch
    (BSL.hPutStrLn h resp >> putStrLn ("Sent: " ++ BSL.unpack resp))
    (\e -> putStrLn $ show (e :: IOException))
    
        
