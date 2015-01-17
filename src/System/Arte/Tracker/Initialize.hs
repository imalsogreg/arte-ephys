{-# LANGUAGE RecordWildCards #-}

module System.Arte.Tracker.Initialize where

import           Control.Concurrent.STM
import           Codec.Picture
import qualified Codec.FFmpeg as FF
import           Options.Applicative

import System.Arte.Tracker.Types


------------------------------------------------------------------------------
makeCamera :: CameraOptions -> IO Camera
makeCamera CameraOptions{..} =
  Camera
  <$> (fst <$> getStream)
  <*> (snd <$> getStream)
  <*> newTVarIO optBackgroundImg
  <*> newTVarIO optCamPos
  where
    (getStream,cleanup) = case optFramesource of
      FFMpegMovie fn = do
        (getFrame, cleanup) <- FF.imageReaderTime fn
        return (Streams.makeInputStream $ Just <$> FF.getFrame f, cleanup)
      FlyCapSSN = error "Implement flycap input"

initialize :: IO TrackerState
initialize 
