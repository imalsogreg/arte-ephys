{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Arte.Tracker.Types where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.STM
import           Data.Aeson
import qualified Data.Foldable        as F
import qualified Data.Map             as M
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Traversable     as T
import qualified System.IO.Streams    as Streams
import           Codec.Picture
import           GHC.Generics
import qualified Linear               as L
------------------------------------------------------------------------------
import           Data.Ephys

------------------------------------------------------------------------------
data TrackerState = TrackerState {   -- TODO Do I use this for anything?
  tsCamGroups :: CamGroups Camera
  } deriving (Show)

data RatPos = RatPos { rpTime :: ExperimentTime
                     , rpPos  :: L.V4 Double }
  deriving (Eq, Show, Read)

type CamName = String
type CamGroupName = String

data CamGroup a = SingleOverhead a
                | MultiCam (M.Map String a)
                deriving (Show, Generic)


newtype CamGroups a = CamGroups (M.Map CamGroupName (CamGroup a))
                      deriving (Show, Generic)

data EstimationOptions = EstOpts {
    blurPx       :: Maybe Double
  , trackHeightM :: Double       --* For use with single overhead camera
  
                                 }

------------------------------------------------------------------------------
makeFrameProducer ::
  CamGroups Camera -> IO (Streams.InputStream (CamGroups (TrackerImage)))
makeFrameProducer gs = Streams.makeInputStream $ getFrames gs

getFrames :: CamGroups Camera -> IO (Maybe (CamGroups (TrackerImage)))
getFrames gs = T.traverse id <$>
                 (T.traverse (Streams.read . frameSource)) gs


data Camera = Camera {
    frameSource        :: Streams.InputStream (TrackerImage)
  , frameSourceCleanup :: IO ()
  , backgroundImg      :: TVar (Maybe (TrackerImage))
  , camPos             :: TVar (Maybe CamPos)
  }

------------------------------------------------------------------------------
data CameraOptions = CameraOptions {
    optFrameSource   :: FrameSource
  , optBackgroundImg :: Maybe FilePath
  , optCamPos        :: Maybe CamPos
  } deriving (Show, Generic)

type TrackerPixel = PixelRGB8
type TrackerImage = Image TrackerPixel

data FrameSource = FFMpegFile FilePath
                 | FlyCapSSN  Integer
                 deriving (Show, Generic)

data CamPos = CamPos {cX   :: Double, cY     :: Double, cZ    :: Double
                     ,cYaw :: Double, cPitch :: Double, cRoll :: Double
                           } deriving (Show, Generic)


data ServerOptions = ServerOptions {
    serverOptsPort :: Int
  } deriving (Eq, Show)


newtype Netstring = Netstring { getByteString :: BSC.ByteString }
                  deriving (Eq, Show)



------------------------------------------------------------------------------
instance ToJSON a => ToJSON (CamGroup a)
instance FromJSON a => FromJSON (CamGroup a)

instance Functor CamGroup where
  fmap f (SingleOverhead a) = SingleOverhead (f a)
  fmap f (MultiCam m)       = MultiCam $ f <$> m


------------------------------------------------------------------------------
instance Applicative CamGroup where
  pure                                  = SingleOverhead
  SingleOverhead f <*> SingleOverhead a = SingleOverhead (f a)
  SingleOverhead f <*> MultiCam       a = MultiCam (f <$> a)
  MultiCam       f <*> SingleOverhead a = MultiCam (($ a) <$> f)
  MultiCam       f <*> MultiCam       a =
    MultiCam (M.intersectionWith ($) f a)


------------------------------------------------------------------------------
instance F.Foldable CamGroup where
  foldMap f (SingleOverhead a) = f a
  foldMap f (MultiCam m)       = F.foldMap f m


------------------------------------------------------------------------------
instance FromJSON a => FromJSON (CamGroups a)
instance ToJSON a   => ToJSON   (CamGroups a)


------------------------------------------------------------------------------
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

instance Show Camera where
  show (Camera _ _ _ _) = "Camera <frameSource> <TVar image> <TVar CamPos>"


instance ToJSON   CameraOptions
instance FromJSON CameraOptions


instance ToJSON   FrameSource
instance FromJSON FrameSource




------------------------------------------------------------------------------
instance ToJSON   CamPos
instance FromJSON CamPos


------------------------------------------------------------------------------
instance ToJSON RatPos where
  toJSON (RatPos t (L.V4 x vX y vY)) =
    object ["t"   .= t
           , "x"  .= x
           , "vx" .= vX
           , "y"  .= y
           , "vy" .= vY
           ]

instance FromJSON RatPos where
  parseJSON (Object v) = RatPos
      <$> (v .: "t")
      <*> (L.V4 <$> v .: "x" <*> v .: "vx" <*> v .: "y" <*> v .: "vy")

