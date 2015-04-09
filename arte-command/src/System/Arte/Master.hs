{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

----------------------------------------------------------------------
-- |
-- Module     : Main
-- Copyright  : (c) Greg Hale 2013
-- License    : BSD3
-- 
-- Maintainer : imalsogreg@gmail.com
-- Stability  : unstable
-- Portability: 
--
-- ArteMaster, A user interface for issuing commands to Arte
--
----------------------------------------------------------------------

module Main where

import Arte.Common
import Arte.Common.Net
import Arte.Common.NetMessage

import System.ZMQ as Z
import qualified Data.ByteString.Char8 as C hiding (putStrLn)
import qualified Data.ByteString as BS
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import System.IO
import Network
import Control.Lens
import Control.Concurrent
import Control.Monad
import Data.Serialize
import Control.Concurrent.Async
import Text.Printf
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as Text
import System.Console.CmdArgs

acceptClients :: Node -> TQueue ArteMessage -> IO ()
acceptClients masterNode requestQueue = case masterNode^.inPort of
    Nothing -> error "Configuration error, master node has no inPort field"
    Just p  -> do
      print $ "Awaiting connections, listening on " ++ show p
      sock <- listenOn (PortNumber . fromIntegral $ p)
      loop sock
        where
          loop s = do
            (handle,clientHost,clientPort) <- accept s
            print $ "Accepted host " ++ clientHost ++ " on port " ++ show clientPort
            withAsync (listenToClient handle requestQueue) (const $ loop s)

listenToClient :: Handle -> TQueue ArteMessage -> IO ()
listenToClient h rQueue = loop
  where loop = do
          m' <- receiveWithSize h
          case m' of
            Left e  -> putStrLn $ unwords ["Got a bad value. error:",e]
            Right m -> do (atomically $ writeTQueue rQueue m)
                          putStrLn $ "Got message: " ++ (take 40 . show . msgBody $ m)
                          case m of
                            ArteMessage _ _ _ (Request ServerHangup) -> return ()
                            _ -> loop

handleRequests :: Node -> TQueue ArteMessage -> IO ()
handleRequests masterNode reqQueue =
  Z.withContext 1 $ \ctx -> do
    Z.withSocket ctx Z.Pub $ \mPubSock -> do
      let pubStr = zmqStr Tcp "*" (show $ masterNode^.port)
      Z.bind mPubSock $ pubStr
      loop mPubSock
  where
    loop :: Z.Socket Z.Pub -> IO ()
    loop mPubSock =  do
      req <- atomically $ readTQueue reqQueue
      case msgBody req of
        Response _ -> loop mPubSock
        Request  r -> do
          continue <- respondTo r mPubSock
          case continue of
            False -> print "Master: Bye!"
            True  -> loop mPubSock

respondTo :: NetRequest -> Z.Socket Z.Pub -> IO Bool
respondTo req outbox = do
  let tNow  = 0 -- TODO : Fix
      nFrom = "master"
      msgs = case req of
        NetPing        -> [ArteMessage tNow nFrom Nothing (Response NetPong)]
        ServerHangup   -> [ArteMessage tNow nFrom Nothing (Response EmptyResponse)]
        ForceQuit      -> [ArteMessage tNow nFrom Nothing (Response EmptyResponse)]
        TrodeSetAllClusters n cs ->
          [ArteMessage tNow nFrom Nothing (Response EmptyResponse)
          , ArteMessage tNow nFrom (Just "decoder") (Request $ TrodeSetAllClusters n cs)
          ]
        StartAcquisition -> [ArteMessage tNow nFrom Nothing (Request StartAcquisition)]
  mapM_ (\m -> Z.send outbox (encode m) [] >> print ("Sent a message:" ++ (take 40 . show . msgBody $ m))) msgs
  case req of
    ServerHangup -> return False
    _            -> return True

  
main :: IO ()
main = do
  opts <- cmdArgs masterCmd
  reqQueue <- newTQueueIO
  m <- getAppNode "master" Nothing
  case m of
    Left e -> error $ "Couldn't read configuration data.  Error detail " ++ e
    Right masterNode ->
      withAsync (acceptClients  masterNode reqQueue) $ \a1 -> do
        withAsync (handleRequests masterNode reqQueue) $ \a2 -> do
          when (waitKeyboard opts) $ do
            putStrLn $ "Type something and hit enter to send StartAcquision out."
            getLine >>= \_ -> return ()
          threadDelay . (1000000 *) . startAcqDelay $ opts
          putStrLn "Master sending Acq command"
          atomically $ writeTQueue reqQueue (ArteMessage 0 "master" Nothing (Request StartAcquisition))
          waitBoth a1 a2
          return ()

data ArteMasterCmd = MasterCmd
  { startAcqDelay :: Int
  , waitKeyboard  :: Bool
  } deriving (Show,Data,Typeable)

masterCmd :: ArteMasterCmd
masterCmd = MasterCmd { startAcqDelay = 0     &= help "Pause n seconds before sending Acq signal"
                      , waitKeyboard  = False &= help "Wait for [Enter] before sending Acq signal"
                      }

--  st <- initState
--  start (masterWindow st)
       
-- Load configuration data (possibly just hosts info)
-- (Startup backend executable on other machine?)
-- (Startup Vis, Tracker on other machine?)
-- Ask Backend for backend configuration
-- Setup command clients
-- Setup listeners to other servers?
-- Setup GUI
-- Poll for ZMQ events and UI events
-- Display received messages
-- Send messages in response to UI (eg, start/stop acq.
--        reset clocks, set 
{-               
data MasterState = 
  MasterState { acquiring    :: Bool
              , disking      :: Bool
              , taskQueue    :: (Seq ArteCommand)
              , messageLog   :: (Seq ArteMessage)
              , nodePorts    :: [Socket ArteMessage]
              }

initState :: IO (TVar MasterState)
initState = atomically $ newTVar 
            (MasterState{ acquiring = False
                        , disking = False
                        , taskQueue = Data.Sequence.empty
                        , messageLog = Data.Sequence.empty
                        , nodePorts = [] 
                        })
-}
{-
data MasterWindowCtrl = 
  MasterWindowCtrl { guiFrame     :: Frame ()
                   , acqButton    :: Button ()
                   , diskButton   :: Button ()
                   , quitButton   :: Button ()
                   , messageList  :: ListCtrl ()
                   } deriving (Show)

entries :: [ (String,String,String) ]
entries = [ ("Hi", "Hey", "Hello") 
          , ("Hi", "Hi", "Hi")
          , ("Yet", "Another", "Entry")
          , ("Just", "One", "More") 
          ]

masterWindow :: (TVar MasterState) -> IO ()
masterWindow stT = do
  f     <- frame [visible := False]
  dskB <- button f [text := "Start Disk"]
  acqB <- button f [text := "Start Acq"]
  rstB <- button f [text := "Reset Clocks"]
  qutB <- button f [text := "Quit"]
  msgL <- listCtrl f [columns := [("Source", AlignCentre, 80)
                                 ,("Timestamp", AlignCentre, 100)
                                 ,("Content", AlignCentre, 100)]
                      , items := [["Hi","Hey","Hello"], ["Hi again","Hey again","Hello again"]]]
  let masterCtrl = MasterWindowCtrl f acqB dskB qutB msgL
  set acqB [on command := acqBHandle stT masterCtrl]
  set dskB [enabled := False, on command := putStrLn "hi"]
  set rstB [on command := putStrLn "clocks"]
  set qutB [on command := putStrLn "quit"]
  set f [layout := margin 8 ( expand (column 2 [ floatCentre (row 4 [ (widget acqB)
                                                                    , (widget dskB)
                                                                    , (widget rstB)
                                                                    , (widget qutB)])
                                               , expand (widget msgL) ]) ) ]
  set f [visible := True]

updateGui :: MasterWindowCtrl -> (TVar MasterState) -> IO ()
updateGui gui stT = do
  st <- atomically $ readTVar stT
  case acquiring st of
    True -> do
      set (acqButton gui) [text := "Stop Acquisition"]
      set (diskButton gui) [enabled := True]
      set (quitButton gui) [enabled := False]
    False -> do
      set (acqButton gui) [text := "Start Acquisition"]
      set (diskButton gui) [enabled := False]
      set (quitButton gui) [enabled := True]
  listCtrlScrollList (messageList gui) (Vector 0 100) >> return ()

acqBHandle :: (TVar MasterState) -> MasterWindowCtrl -> IO ()
acqBHandle stT gui = do 
  atomically $ modifyTVar stT (\s -> s {acquiring = not (acquiring s)})
  newTestItem "A" "Little" "Test" gui
  updateGui gui stT
         
dskBHandle :: (TVar MasterState) -> IO ()
dskBHandle = undefined


newTestItem :: String -> String -> String -> MasterWindowCtrl -> IO ()
newTestItem src ts content gui = do
  itemAppend (messageList gui) [src,ts,content]

               
runCom :: (TVar MasterState) -> IO ()
runCom masterState = do
  withContext 1 $ \context -> do
    withSocket context Req $ \cliSock -> do
      withSocket context Rep $ \servSock -> do
        bind servSock servStr
        connect cliSock cliStr
        
          
  where cliStr = zmqStr Tcp "127.0.0,1" "5224"
        servStr = zmqStr Tcp "127.0.0.1" "5223"

sendSimpleMessage :: Z.Socket Req -> IO ()
sendSimpleMessage sock = do
  let req = C.pack "SimpleMessage"
  send sock req []
  rep <- receive sock []
  putStrLn $ "Got response: " ++ C.unpack rep
-}
