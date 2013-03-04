----------------------------------------------------------------------
-- |
-- Module     : Main
-- Copyright  : (c) Greg Hale 2013
-- License    : GPL-3
-- 
-- Maintainer : imalsogreg@gmail.com
-- Stability  : unstable
-- Portability: not portable, uses posix & linux kernel modules
--
-- ArteMaster, A user interface for issuing commands to Arte
--
----------------------------------------------------------------------

module Main where


main :: IO ()
main = undefined
-- Load configuration data (possibly just hosts info)
-- (Startup backend executable on other machine?)
-- (Startup Vis, Tracker on other machine?)
-- Ask Backend for backend configuration
-- Setup command server
-- Letup listeners to other servers
-- Setup GUI
-- Poll for ZMQ events and UI events
-- Display received messages
-- Send messages in response to UI (eg, start/stop acq.
--        reset clocks, set 