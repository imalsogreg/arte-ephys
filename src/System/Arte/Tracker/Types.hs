{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}


module System.Arte.Tracker.Types where

import           Control.Applicative
import           Control.Concurrent.STM
import           Data.Aeson
import qualified Data.Foldable        as F
import qualified Data.Map             as M
import qualified Data.Traversable     as T
import qualified System.IO.Streams    as Streams
import           Codec.Picture
import           GHC.Generics

type CamName = String
type CamGroupName = String

data CamGroup a = SingleOverhead a
                | MultiCam (M.Map String a)
                deriving (Show, Generic)

instance ToJSON a => ToJSON (CamGroup a)
instance FromJSON a => FromJSON (CamGroup a)

instance Functor CamGroup where
  fmap f (SingleOverhead a) = SingleOverhead (f a)
  fmap f (MultiCam m)       = MultiCam $ f <$> m

------------------------------------------------------------------------------
instance Applicative CamGroup where
  pure a                                = SingleOverhead a
  SingleOverhead f <*> SingleOverhead a = SingleOverhead (f a)
  SingleOverhead f <*> MultiCam       a = MultiCam (f <$> a)
  MultiCam       f <*> SingleOverhead a = MultiCam (($ a) <$> f)
  MultiCam       f <*> MultiCam       a =
    MultiCam (M.intersectionWith ($) f a)

instance F.Foldable CamGroup where
  foldMap f (SingleOverhead a) = f a
  foldMap f (MultiCam m)       = F.foldMap f m

newtype CamGroups a = CamGroups (M.Map CamGroupName (CamGroup a))
                      deriving (Show, Generic)

instance FromJSON a => FromJSON (CamGroups a)
instance ToJSON a   => ToJSON   (CamGroups a)

newtype CamOptions = CamOptions (CamGroups Int) deriving (Generic)


instance Functor CamGroups where
  fmap f (CamGroups a) = CamGroups ((fmap.fmap ) f a)

instance Applicative CamGroups where
  pure a = CamGroups (M.fromList [("default",SingleOverhead a)])
  (CamGroups f) <*> (CamGroups a) = CamGroups (M.intersectionWith (<*>) f a)

instance F.Foldable CamGroups where
  foldMap f (CamGroups a) = F.foldMap (F.foldMap f) a

instance T.Traversable CamGroups where
  traverse f (CamGroups a) = CamGroups <$> (T.traverse . T.traverse) f a

------------------------------------------------------------------------------
instance T.Traversable CamGroup where
  traverse f (SingleOverhead a) = SingleOverhead <$> (f a)
  traverse f (MultiCam a)       = MultiCam       <$> T.traverse f a


getFrames :: CamGroups Camera -> IO (Maybe (CamGroups (TrackerImage)))
getFrames gs = T.traverse id <$>
                 (T.traverse (Streams.read . frameSource)) gs

type TrackerPixel = PixelRGB8
type TrackerImage = Image TrackerPixel

------------------------------------------------------------------------------
makeFrameProducer :: 
  CamGroups Camera -> IO (Streams.InputStream (CamGroups (TrackerImage))) 
makeFrameProducer gs = Streams.makeInputStream $ getFrames gs

data Camera = Camera {
    frameSource        :: Streams.InputStream (TrackerImage)
  , frameSourceCleanup :: IO ()
  , backgroundImg      :: TVar (Maybe (TrackerImage))
  , camPos             :: TVar (Maybe CamPos)
  } 

instance Show Camera where
  show (Camera _ _ _ _) = "Camera <frameSource> <TVar image> <TVar CamPos>"

------------------------------------------------------------------------------
data CameraOptions = CameraOptions {
    optFrameSource   :: FrameSource
  , optBackgroundImg :: Maybe FilePath
  , optCamPos        :: Maybe CamPos
  } deriving (Show, Generic)

instance ToJSON   CameraOptions
instance FromJSON CameraOptions

data FrameSource = FFMpegFile FilePath
                 | FlyCapSSN  Integer
                 deriving (Show, Generic)

instance ToJSON   FrameSource
instance FromJSON FrameSource


data CamPos = CamPos {cX   :: Double, cY     :: Double, cZ    :: Double
                     ,cYaw :: Double, cPitch :: Double, cRoll :: Double
                     } deriving (Show, Generic)

instance ToJSON   CamPos
instance FromJSON CamPos

data TrackerState = TrackerState {
  tsCamGroups :: CamGroups Camera
  } deriving (Show)

