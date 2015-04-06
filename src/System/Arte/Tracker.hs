{-# LANGUAGE OverloadedStrings #-}

module System.Arte.Tracker where

import qualified Codec.Picture as JP
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Error hiding (initZ)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Time
import Data.Traversable
import Linear (V4(..))
import qualified System.Remote.Monitoring as EKG
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.FilePath
import System.Environment
import qualified System.IO.Streams as Streams
import System.Arte.Tracker.Types
import System.Arte.Tracker.State
import System.Arte.Tracker.Initialize
import System.Arte.Tracker.Server
import System.Arte.Tracker.Kalman
import qualified System.Arte.Tracker.Kalman as Kalman

runTracker :: IO ()
runTracker = do

  EKG.forkServer "localhost" 8080       -- Remote process monitor

  ini <- runEitherT $ do
    camOpts <- readCamOpts
    cams <- initializeCams camOpts
    return (camOpts, cams)

  either' ini (putStrLn . ("Initialization errer: " ++)) $ \(opts,cams) -> do
    st  <- makeState opts
    frameSource <- makeFrameProducer cams
    async (handleFrames (initZ, initP) st frameSource)
    playIO (InWindow "Tracker" (500,500) (10,10)) black 300
      draw input timeStep

handleFrames :: Kalman.State
             -> TVar DisplayState
             -> Streams.InputStream (CamGroup TrackerImage)
             -> IO ()
handleFrames kState ds' frameStream = do
  f      <- Streams.read frameStream
  case f of
    Nothing -> return ()
    Just frames -> do
      (k',pos) <- stepKalman (1/30) kState <$> estimatePos frames
      writeChan posChan thisPos
      atomically $ modifyTVar ds' (\ds -> ds {thisFrame = frames})
      handleFrames (k', pos) ds' frameStream

draw :: DisplayState -> IO Picture
draw (DisplayState frm sel kalmanS) = undefined

input :: Event -> DisplayState -> IO DisplayState
input _ _ = undefined

timeStep :: Float -> IO DisplayState
timeStep = undefined

makeState :: CamGroups CameraOptions -> IO (TVar DisplayState)
makeState opts@(CamGroups m) = do
  imgs <- traverse  (\opt -> JP.generateImage (\x y -> 0) 100 100) opts
  let groupName = head . Map.keys $ m
      headGroup = Map.lookup groupName
      camName = case headGroup of
        Nothing -> error "Impossible case - empty opts"
        Just (SingleOverhead a) -> Nothing
        Just (MultiCam gm) -> Just . head . Map.keys $ gm
      sel = (groupName, camName)
  return . newTVarIO $ DisplayState imgs sel


readCamOpts :: EitherT String IO (CamGroups CameraOptions)
readCamOpts = do
  home <- noteT "No $HOME environment variable" (MaybeT (lookupEnv "HOME"))
  EitherT (eitherDecode <$> BSL.readFile (home </> ".arte-ephys/tracker.conf"))

either' :: Either l r -> (l -> m a) -> (r -> m a) -> m a
either' e l r = either l r e
{-
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
-}

