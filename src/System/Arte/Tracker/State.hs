module System.Arte.Tracker.State where

import System.Arte.Tracker.Types

data DisplayState = DisplayState {
    thisFrame :: CamGroups TrackerImage
  , camSelection :: (CamGroupName,Maybe CamName)
  }
