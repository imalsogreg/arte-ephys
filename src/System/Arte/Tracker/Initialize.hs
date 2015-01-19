{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Arte.Tracker.Initialize where

------------------------------------------------------------------------------
import           Control.Concurrent.STM
import           Control.Error
import           Control.Error.Util
import           Control.Monad.Trans 
import           Codec.Picture
import qualified Codec.FFmpeg           as FF
import qualified Data.Traversable       as T
import           Options.Applicative
import qualified System.IO.Streams      as Streams
------------------------------------------------------------------------------
import System.Arte.Tracker.Types


------------------------------------------------------------------------------
makeCamera :: CameraOptions -> IO (Either String Camera)
makeCamera CameraOptions{..} = runEitherT $ do

  (getStream, cleanup) <- do
      case optFrameSource of
        FFMpegFile fn -> do
          (g, c) <- (lift $ FF.imageReader fn) :: EitherT String IO (IO (Maybe TrackerImage), IO ())
          s <- lift $ Streams.makeInputStream g
          return ((s :: Streams.InputStream TrackerImage) , (c :: IO ()))
        FlyCapSSN _ -> error "Implement flycap input"

  bkgnd <- EitherT $ initImage optBackgroundImg
  p     <- lift $ newTVarIO optCamPos

  return $ Camera getStream cleanup bkgnd p



initImage :: Maybe FilePath
             -> IO (Either String (TVar (Maybe TrackerImage)))
initImage Nothing   = Right <$> newTVarIO Nothing
initImage (Just fn) = do
  res <- readImage fn
  case res of
    Left e  -> return (Left e)
    Right i -> Right <$> newTVarIO (Just i)


--initialize :: IO TrackerState
--initialize = TrackerState <$> T.forM exampleInput makeCamera
