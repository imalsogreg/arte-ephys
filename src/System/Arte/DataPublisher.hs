{-# LANGUAGE TemplateHaskell #-}

module System.Arte.DataPublisher where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import Network
import System.IO
import Text.Printf

import System.Arte.Net

data DataPublisher a =
  DataPublisher { _chan        :: TQueue a
                , _subscribers :: TVar [Handle] }
$(makeLenses ''DataPublisher)

runPublisher :: (Show a, S.Serialize a) => DataPublisher a -> IO ()
runPublisher pub = forever $ do
  a <- atomically $ readTQueue (pub^.chan)
  let packet = toPacket a :: BS.ByteString
  subs <- atomically . readTVar $ pub^.subscribers
  putStrLn $ "Pushing " ++ show a  ++ " to " ++ show subs
  forM_ subs $ flip sendLengthTaggedPacket packet

------------------------------------------------------------------------------
-- |Listen for clients wanting a subscription
acceptSubscribers :: Node -> DataPublisher a -> IO ()
acceptSubscribers me pub = withSocketsDo $ do
  let topPort = me ^. nodeServerPort
  sock <- listenOn (PortNumber $ fromIntegral topPort)
  _ <- printf "%s listeting on port %d\n" (me^.nodeName) topPort
  forever $ do
    (handle, host, port) <- accept sock
    _ <- printf "%s accepted connection from %s" host (show port)
    hSetBinaryMode handle True
    hSetBuffering handle NoBuffering
--    forkFinally (atomically $ addSubscriber pub handle) (\_ -> hClose handle)
    atomically $ addSubscriber pub handle

addSubscriber :: DataPublisher a -> Handle -> STM ()
addSubscriber pub h = modifyTVar (pub^.subscribers) (h:)

withSubscription :: (S.Serialize a) => Node -> (Either String a -> IO b) -> IO ()
withSubscription node action = do
  h <- connectTo (node^.nodeHost.hostIp)
       (PortNumber . fromIntegral $ node^.nodeServerPort)
  putStrLn "Got a connection"
  hSetBinaryMode h True
  hSetBuffering h NoBuffering
  forever $ action =<< recvData h

