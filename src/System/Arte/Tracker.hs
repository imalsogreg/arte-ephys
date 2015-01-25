module System.Arte.Tracker where

import System.Arte.Tracker.Types
import System.Arte.Tracker.Initialize

runTracker :: IO ()
runTracker = do
  cs <- initialize exampleInput
  print cs
