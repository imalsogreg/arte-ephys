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

main :: IO ()
main = setupWindow
-- Load configuration data (possibly just hosts info)
-- (Startup backend executable on other machine?)
-- (Startup Vis, Tracker on other machine?)
-- Ask Backend for backend configuration
-- Setup command server
-- Setup listeners to other servers?
-- Setup GUI
-- Poll for ZMQ events and UI events
-- Display received messages
-- Send messages in response to UI (eg, start/stop acq.
--        reset clocks, set 

type TransportName   = String
type HostName        = String
type PortName        = String
type ZmqSocketString = String

zmqSocketString :: TransportName -> HostName -> PortName -> ZmqSocketString
zmqSocketString t h p = t ++ "://" ++ h ++ ":" ++ p

setupCommandClient :: TransportName -> HostName -> PortName -> IO (Z.Req)
setupCommandClient = undefined

setupWindow :: IO ()
setupWindow = do
  initGUI
  window <- windowNew
  window `on` deleteEvent $ liftIO mainQuit >> return False
  window `on` configureEvent $ do
    (width, height) <- eventSize
    liftIO (putStrLn (show width ++ " x " ++ show height))
    return False
  widgetShowAll window
  mainGUI