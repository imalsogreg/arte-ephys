{-# LANGUAGE OverloadedStrings #-}

module System.Arte.Tracker where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Error
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Time
import Linear (V4(..))
import System.Arte.Tracker.Types
import System.Arte.Tracker.Initialize
import System.Arte.Tracker.Server

runTracker :: IO ()
runTracker = do

  --cs     <- runEitherT $ initializeFromFile "/home/greghale/.arte-ephys/tracker.conf"
  --print cs

  -- /////Temporary stuff////// --
  tStart <- getCurrentTime
  posChan <- newChan
  server <- async $ serveFromSingleCamera (ServerOptions 5555) posChan
  let testPos t = RatPos (realToFrac $ diffUTCTime t tStart) (V4 1 2 3 4)
  forever $ do
    thisPos <- testPos <$> getCurrentTime
    BSL.putStrLn $ BSL.concat ["Writing: ", encode thisPos]
    writeChan posChan thisPos
    threadDelay 33000
    
  
