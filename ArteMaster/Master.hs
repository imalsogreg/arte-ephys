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
import Graphics.UI.Gtk
import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 as C hiding (putStrLn)
import Control.Concurrent
import Control.Concurrent.STM
import Data.Sequence
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
               
data MasterState = MSt { acquiring    :: Bool
                       , disking      :: Bool
                       , taskQueue    :: (Seq ArteCommand)
                       , messageLog   :: (Seq ArteMessage)
                       } deriving (Show)

--data MasterStateT = TVar MasterState

initState :: STM (TVar MasterState)
initState = newTVar $ MSt{ acquiring = False
                         ,  disking = False
                         ,  taskQueue = Data.Sequence.empty
                         ,  messageLog = Data.Sequence.empty
                         }

main :: IO ()
main = do
  setupWindow
       
setupWindow :: IO ()
setupWindow = do
  _ <- initGUI
  window <- windowNew
  hb     <- hBoxNew True 2
  send_b <- buttonNew
  buttonSetLabel send_b "Send"
  quit_b <- buttonNew
  buttonSetLabel quit_b "Quit"
  _ <- onClicked send_b ( putStrLn "send clicked")
  boxPackStart hb send_b PackGrow 0
  boxPackStart hb quit_b PackGrow 1
  window `containerAdd` hb
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False
  _ <- window `on` configureEvent $ do
    (width, height) <- eventSize
    liftIO (putStrLn (show width ++ " x " ++ show height))
    return False
  widgetShowAll window
  mainGUI
  

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