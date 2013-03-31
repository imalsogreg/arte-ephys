----------------------------------------------------------------------
-- |
-- Module     : Main
-- Copyright  : (c) Greg Hale 2013
-- License    : GPL-3
-- 
-- Maintainer : imalsogreg@gmail.com
-- Stability  : unstable
-- Portability: not portable. Tries to launch 
--
-- ArteMaster, A user interface for issuing commands to Arte
--
----------------------------------------------------------------------

module Main where

import System.ZMQ as Z
import Graphics.UI.WX
import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 as C hiding (putStrLn)
import Control.Concurrent
import Control.Concurrent.STM
import Data.Sequence
import Data.Maybe
import ZmqUtils
import ArteBase

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
               
data MasterState = 
  MasterState { acquiring    :: Bool
              , disking      :: Bool
              , taskQueue    :: (Seq ArteCommand)
              , messageLog   :: (Seq ArteMessage)
--              , guiCtrl      :: Maybe MasterWindowCtrl
              } deriving (Show)

--data MasterStateT = TVar MasterState

initState :: IO (TVar MasterState)
initState = atomically $ newTVar 
            (MasterState{ acquiring = False
                        ,  disking = False
                        ,  taskQueue = Data.Sequence.empty
                        ,  messageLog = Data.Sequence.empty
--                        ,  guiCtrl = Nothing
                        })

main :: IO ()
main = do
  st <- initState
  start (masterWindow st)
       
data MasterWindowCtrl = 
  MasterWindowCtrl { guiFrame     :: Frame ()
                   , acqButton    :: Button ()
                   , diskButton   :: Button ()
                   , messageList  :: ListCtrl ()
                   } deriving (Show)


masterWindow :: (TVar MasterState) -> IO ()
masterWindow st = do
  f     <- frame [visible := False]
  dskB <- button f [text := "Start Disk", on command := putStrLn "hello"]
  acqB <- button f [text := "Start Acq", on command := putStrLn "hi"]  
  rstB <- button f [text := "Reset Clocks", on command := putStrLn "clocks"]
  qutB <- button f [text := "Quit", on command := putStrLn "quit"]
  msgL <- listCtrl f [columns := [("Source", AlignCentre, 80)
                                 ,("Timestamp", AlignCentre, 100)
                                 ,("Content", AlignCentre, 100)]]    
  let masterCtrl = MasterWindowCtrl f acqB dskB msgL
  set dskB [on command := putStrLn "hello"]
  set acqB [on command := putStrLn "hi"]
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
    False -> do
      set (acqButton gui) [text := "Start Acquisition"]
      set (diskButton gui) [enabled := True]

acqBHandle :: (TVar MasterState) -> MasterWindowCtrl -> IO ()
acqBHandle stT gui = do
  disk_on <- atomically $ do
    st <- readTVar stT
    return (disking st)
  if disk_on then
    putStrLn "Can't stop acquisition with disk on."
     else
    atomically (toggleAcq stT)
         
{-
toggleAcq :: (TVar MasterState) -> STM ()
toggleAcq stT = do
  st <- readTVar stT
  let gui = fromJust (guiCtrl st)
  set (acqButton gui) [text := "Stop Acquisition"]
  -}

dskBHandle :: (TVar MasterState) -> IO ()
dskBHandle = undefined
    
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