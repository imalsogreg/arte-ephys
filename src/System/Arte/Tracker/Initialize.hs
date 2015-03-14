{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Arte.Tracker.Initialize where

--- * imports
------------------------------------------------------------------------------
import           Control.Concurrent.STM
import           Control.Error
import           Control.Error.Util
import           Control.Monad.Trans
import           Codec.Picture
import qualified Codec.FFmpeg           as FF
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BS
import qualified Data.Map               as M
import qualified Data.Traversable       as T
import           Options.Applicative
import qualified System.IO.Streams      as Streams
------------------------------------------------------------------------------
import System.Arte.Tracker.Types


------------------------------------------------------------------------------
makeCamera :: CameraOptions -> IO (Either String Camera)
makeCamera CameraOptions{..} = runEitherT $ do

  (getStream, cleanup) <- case optFrameSource of
        FFMpegFile fn -> do
          (g, c) <- (lift $ FF.imageReader fn) :: EitherT String IO (IO (Maybe TrackerImage), IO ())
          s <- lift $ Streams.makeInputStream g
          return ((s :: Streams.InputStream TrackerImage) , (c :: IO ()))
        FlyCapSSN _ -> error "Implement flycap input"

  bkgnd <- EitherT $ initImage optBackgroundImg
  p     <- lift $ newTVarIO optCamPos

  return $ Camera getStream cleanup bkgnd p


------------------------------------------------------------------------------
initImage :: Maybe FilePath
             -> IO (Either String (TVar (Maybe TrackerImage)))
initImage Nothing   = Right <$> newTVarIO Nothing
initImage (Just fn) = do
  res <- readImage fn
  case res of
    Left e  -> return (Left e)
    Right (ImageRGB8 i) -> Right <$> newTVarIO (Just i)
    Right _             -> return $
                           Left "Stream delivered wrong type of image."


------------------------------------------------------------------------------
initializeFromFile :: FilePath -> EitherT String IO (CamGroups Camera)
initializeFromFile fp = EitherT (eitherDecode <$> BS.readFile fp) >>=
                        initializeCams

initializeCams :: CamGroups CameraOptions -> EitherT String IO (CamGroups Camera)
initializeCams camOpts = EitherT $ T.sequence <$> T.mapM makeCamera camOpts


------------------------------------------------------------------------------
exampleInput :: CamGroups CameraOptions
exampleInput = CamGroups $ M.fromList
               [("track",    SingleOverhead trackCamOptions)
               ,("sleepbox", SingleOverhead sleepCamOptions)]
  where
    trackCamOptions =
      CameraOptions { optFrameSource = FFMpegFile "track.avi"
                    , optBackgroundImg = Nothing
                    , optCamPos = Just (CamPos 0 0 10 0 (-pi/2) 0)
                    }
    sleepCamOptions =
      CameraOptions { optFrameSource = FFMpegFile "sleep.avi"
                    , optBackgroundImg = Nothing
                    , optCamPos = Just (CamPos 10 0 10 0 (-pi/2) 0)
                    }

