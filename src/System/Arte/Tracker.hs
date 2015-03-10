module System.Arte.Tracker where

import Control.Error
import System.Arte.Tracker.Types
import System.Arte.Tracker.Initialize
import System.Arte.Tracker.Server

runTracker :: IO ()
runTracker = do
  cs <- runEitherT $ initializeFromFile "/home/greghale/.arte-ephys/tracker.conf"
  print cs
