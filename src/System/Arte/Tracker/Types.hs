{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving, DeriveFunctor, ScopedTypeVariables #-}

module System.Arte.Tracker.Types where

import           Control.Applicative
import           Control.Monad
import           Data.Data
import qualified Data.Foldable       as F
import qualified Data.Map            as M
import qualified Data.Traversable    as T
import           Data.Typeable
import qualified System.IO.Streams   as Streams
import           Codec.Picture
import           GHC.Generics

type CamName = String
type CamGroupName = String

data CamGroup a = SingleOverhead a
                | MultiCam (M.Map String a)
                  

instance Functor CamGroup where
  fmap f (SingleOverhead a) = SingleOverhead (f a)
  fmap f (MultiCam m)       = MultiCam $ f <$> m

------------------------------------------------------------------------------
instance Applicative CamGroup where
  pure a                                = SingleOverhead a
  SingleOverhead f <*> SingleOverhead a = SingleOverhead (f a)
  MultiCam       f <*> MultiCam       a = MultiCam (M.intersectionWith ($) f a)
  SingleOverhead f <*> MultiCam       a = MultiCam (f <$> a)
  MultiCam       f <*> SingleOverhead a = MultiCam (($ a) <$> f)

instance F.Foldable CamGroup where
  foldMap f (SingleOverhead a) = f a
  foldMap f (MultiCam m)       = F.foldMap f m

newtype CamGroups a = CamGroups (M.Map CamGroupName (CamGroup a))

instance Functor CamGroups where
  fmap f (CamGroups a) = CamGroups ((fmap.fmap ) f a)

instance Applicative CamGroups where
  pure a = CamGroups (M.fromList [("default",SingleOverhead a)])
  (CamGroups f) <*> (CamGroups a) = CamGroups (M.intersectionWith (<*>) f a)

b :: (Bool -> [Int]) -> CamGroups Bool -> [Int]
b = F.foldMap

r :: [Int]
r = F.foldMap _ (undefined :: CamGroup Bool)

-- foldMap :: Monoid m => (a -> m) -> CamGroups a -> m
instance F.Foldable CamGroups where
  foldMap f (CamGroups a) = _ (M.elems a)

------------------------------------------------------------------------------
instance T.Traversable CamGroup where
  traverse f (SingleOverhead a) = SingleOverhead <$> (f a)
  traverse f (MultiCam a)       = MultiCam       <$> T.traverse f a

liftFrameOk :: CamGroup (Maybe DynamicImage) -> Maybe (CamGroup DynamicImage)
liftFrameOk = T.traverse id 

getFrames :: CamGroup Camera -> IO (CamGroup (Maybe DynamicImage))
getFrames = T.traverse (Streams.read . frameSource)

getFrames' :: CamGroup Camera -> IO (Maybe (CamGroup DynamicImage))
getFrames' g = liftFrameOk <$> getFrames g

{-
getFrames''    :: CamGroups Camera -> IO (CamGroups (Maybe DynamicImage))
getFrames'' gs =  T.mapM getFrames gs

getFrames''' :: CamGroups Camera -> IO (Maybe (CamGroups DynamicImage))
getFrames''' gs = (T.traverse . T.traverse) id <$> getFrames'' gs
-}

------------------------------------------------------------------------------
makeFrameProducer ::
  CamGroup Camera -> IO (Streams.InputStream (CamGroup DynamicImage))
makeFrameProducer (SingleOverhead c) = do
  undefined
--  Streams.map (SingleOverheadFrame) (frameSource c)
makeFrameProducer (MultiCam m) = do
--  images <- T.forM m Streams.read
  undefined

data Camera = Camera {
  frameSource :: Streams.InputStream DynamicImage
  }

data TrackerState = TrackerState {
  tsCamGroups :: M.Map CamGroupName (CamGroup Camera)
  }
