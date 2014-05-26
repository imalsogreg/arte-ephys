module System.Arte.CommandPort where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Concurrent
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network
import System.IO
import Text.Printf

import System.Arte.Net

acceptClients :: Node -> (Maybe Object -> IO Object) -> IO ()
acceptClients rep talker = do
  sock <- listenOn (PortNumber $ fromIntegral (rep^.nodeServerPort))
  forever $ do
    (handle,host,port) <- accept sock
    _ <- printf "%s accepted command connection from %s\n" host (show port)
    hSetBuffering handle LineBuffering
    forkFinally (talk handle talker) (\_ -> hClose handle)

talk :: Handle -> (Maybe Object -> IO Object) -> IO ()
talk h talker = do
  l <- BS.hGetLine h
  resp <- talker $ decode (BSL.fromChunks [l]) :: IO Object
  catch
    (BSL.hPutStrLn h. encode $ resp)
    (\e -> putStrLn $ show (e :: IOException))
        