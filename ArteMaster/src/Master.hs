{-# LANGUAGE DeriveGeneric #-}

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

acceptClients :: Node -> TChan NetRequest -> IO ()
acceptClients masterNode requestChan = case masterNode^.inPort of
    Nothing -> error "Configuration error, master node has no inPort field"
    Just p  -> do
      print $ "Awaiting connections, listening on " ++ show p
      sock <- listenOn (PortNumber . fromIntegral $ p)
      forever $ do
        (handle,clientHost,clientPort) <- accept sock
        print $ "Accepted host " ++ clientHost ++ " on port " ++ show clientPort
        forkFinally (listenToClient handle requestChan)
          (\_ -> hClose handle)

listenToClient :: Handle -> TChan NetRequest -> IO ()
listenToClient h rChan = do
  hSetBuffering h NoBuffering
  forever $ do
    print $ "About to hGet"
    hop <- hIsOpen h
    hread <- hIsReadable h
    putStrLn $ unwords ["Open",show hop," Readable",show hread]
    m' <- receiveWithSize h
    case m' of
      Left e  -> putStrLn $ "Got a bad value. " ++ e
      Right m -> (atomically $ writeTChan rChan m)

handleRequests :: Node -> TChan NetRequest -> IO ()
handleRequests masterNode reqChan =
  Z.withContext 1 $ \ctx -> do
    Z.withSocket ctx Z.Pub $ \mPubSock -> do
      let pubStr = zmqStr Tcp "*" (show $ masterNode^.port)
      print $ "About to bind pub port: " ++ pubStr
      Z.bind mPubSock $ pubStr
      loop mPubSock
  where
    loop mPubSock =  do
      print $ "Waiting for a value to come to the reqChan"
      req <- atomically $ readTChan reqChan
      print $ "Got a value"
      response <- respondTo req
      print $ "About to send response " ++ (show response)
      Z.send mPubSock (encode response) []
      print $ "Sent response"
      case req of
        ForceQuit -> print "Master: Bye!"
        _         -> loop mPubSock

respondTo :: NetRequest -> IO NetResponse
respondTo req = case req of
  NetPing   -> return NetPong
  ForceQuit -> return EmptyResponse

main :: IO ()
main = do
  reqChan <- newTChanIO
  m <- getAppNode "master" Nothing
  case m of
    Left e -> error $ "Couldn't read configuration data.  Error detail" ++ e
    Right masterNode -> async (acceptClients masterNode reqChan) >>= \a ->
                        handleRequests masterNode reqChan >>
                        wait a
                        

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