module System.Arte.Tracker.Video where

import OpenCV.HighCV
import System.Arte.Tracker.Types

estimatePos :: CamGroup TrackerImage -> RatPos
estimatePos = 
