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
import Graphics.UI.WXCore
import Graphics.UI.WX
import Data.ByteString.Char8 as C hiding (putStrLn)
import Control.Concurrent.STM
import Data.Sequence
import ZmqUtils
import ArteCommon



main :: IO ()
main = do
  st <- initState
  start (masterWindow st)
       
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