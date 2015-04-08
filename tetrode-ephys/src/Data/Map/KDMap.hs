{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Map.KDMap where

import Control.Applicative
import Data.Maybe (maybeToList)
import Data.Monoid
import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Foldable as F

data KDMap k a = KDEmpty
               | KDLeaf   k a Depth
               | KDBranch k a Depth (KDMap k a) (KDMap k a)
               deriving (Eq, Show)

newtype Weight = Weight {unWeight :: Double}
                 deriving (Show, Eq, Num, Enum, Fractional,Real,Ord)

newtype Depth = Depth {unDepth :: Int}
              deriving (Eq,Show,Num,Enum,Integral,Real,Ord)

class KDKey k where
  pointD       :: k -> Depth -> Double
  pointW       :: k -> Weight
  pointSize    :: k -> Depth

  pointDDistSq :: k -> k -> Depth -> Double
  pointDDistSq a b i = (pointD b i - pointD a i)^(2::Int)

  pointDistSq  :: k -> k -> Double
  pointDistSq a b = sum $ map (pointDDistSq a b)
                    [0..pointSize a - 1]

  dimOrder :: k -> k -> Depth -> Ordering
  dimOrder a b n = compare (pointD a n) (pointD b n)

  dSucc :: k -> Depth -> Depth
  dPred :: k -> Depth -> Depth

instance F.Foldable (KDMap k) where
  foldr _ z KDEmpty = z
  foldr f z (KDLeaf _ a _) = f a z
  foldr f z (KDBranch _ v _ kdLeft kdRight) =
    F.foldr f (f v (F.foldr f z kdLeft)) kdRight

data Point2 = Point2 {p2x :: Double
                     ,p2y :: Double
                     ,p2w :: Weight
                     }
            deriving (Show, Eq)

data Point4 = Point4 {p4a :: Double
                     ,p4b :: Double
                     ,p4c :: Double
                     ,p4d :: Double
                     ,p4w :: Weight
                     }
            deriving (Show, Eq)

instance KDKey Point2 where
  pointD p 0 = p2x p
  pointD p 1 = p2y p
  pointD _ n = error $ "Point2 out of bounds index: " ++ show n
  pointSize _    = 2
  pointW p   = p2w p
  dSucc p d = succ d `mod` fromIntegral (pointSize p)
  dPred p d = pred d `mod` fromIntegral (pointSize p)

instance KDKey Point4 where
  pointD p 0 = p4a p
  pointD p 1 = p4b p
  pointD p 2 = p4c p
  pointD p 3 = p4d p
  pointD _ n = error $ "Point4 out of bounds index: " ++ show n
  pointSize p = 4
  pointW p = p4w p
  dSucc p d = succ d `mod` fromIntegral (pointSize p)
  dPred p d = pred d `mod` fromIntegral (pointSize p)

instance Monoid Point2 where
  mempty        = Point2 0 0 0
  a `mappend` b = Point2 x' y' w'
    where w' = p2w a + p2w b
          aFrac = realToFrac $ p2w a / w' :: Double
          bFrac = realToFrac $ p2w b / w' :: Double
          x' = p2x a * aFrac + p2x b * bFrac :: Double
          y' = p2y a * aFrac + p2y b * bFrac :: Double

instance Monoid Point4 where
  mempty        = Point4 0 0 0 0 0
  a `mappend` b = Point4 a' b' c' d' w'
    where w' = p4w a + p4w b
          aFrac = realToFrac $ p4w a / w' :: Double
          bFrac = realToFrac $ p4w b / w'
          x' n = pointD a n * aFrac + pointD b n * bFrac
          (a', b', c', d') = (x' 0, x' 1, x' 2, x' 3)

toList :: KDMap k a -> [(k,a)]
toList KDEmpty = []
toList (KDLeaf k a _) = [(k,a)]
toList (KDBranch k a _ kdLeft kdRight) = (k,a) : toList kdLeft ++ toList kdRight

closer :: (Eq k, KDKey k) => Maybe (k,a) -> Maybe (k,a) -> k -> Maybe (k,a)
closer Nothing Nothing _ = Nothing
closer a Nothing _ = a
closer Nothing b _ = b
closer (Just optA@(kA,_)) (Just optB@(kB,_)) k
  | pointDistSq kA k < pointDistSq kB k = Just optA
  | otherwise                           = Just optB

add :: (Eq k, Eq a, Monoid a, KDKey k, Monoid k) => KDMap k a -> Double -> k -> a -> KDMap k a
add m thresh k a = case closest k m of
  Nothing      -> insert 0 k a m
  Just (k',a') -> if pointDistSq k k' <= thresh^(2::Int)
                  then insert 0 (k <> k') (a <> a') . delete k' $ m
                  else insert 0 k a m

delete :: (Eq k, KDKey k) => k -> KDMap k a -> KDMap k a
delete _ KDEmpty = KDEmpty
delete k m@(KDLeaf k' _ _)
  | k == k'   = KDEmpty
  | otherwise = m
delete k (KDBranch k' a' d kdLeft kdRight)
  | k == k' = fromListWithDepth d (toList kdLeft ++ toList kdRight)
  | otherwise = case dimOrder k k' d of
    EQ -> KDBranch k' a' d (delete k kdLeft) (delete k kdRight)
    LT -> KDBranch k' a' d (delete k kdLeft)  kdRight
    GT -> KDBranch k' a' d  kdLeft           (delete k kdRight)

closest :: (Eq a, Eq k, KDKey k) => k -> KDMap k a -> Maybe (k,a)
closest _ KDEmpty = Nothing
closest _ (KDLeaf k' a' _) = Just (k',a')
closest k (KDBranch k' a' d' kdLeft kdRight) = case dimOrder k k' d' of
  LT -> findNearest kdLeft  kdRight
  _  -> findNearest kdRight kdLeft
  where
    findNearest treeA treeB =
      let mainCandidates = case closest k treeA of
            Nothing  -> [(k',a')]
            Just (k'',a'') -> [(k',a'),(k'',a'')]
          otherCandidates
            | (pointDistSq k k') >= (pointD k d' - pointD k' d')^(2::Int) =
              maybeToList (closest k treeB)
            | otherwise = []
      in Just $ L.minimumBy (comparing (pointDistSq k . fst))
         (mainCandidates ++ otherCandidates)


------------------------------------------------------------------------------
allInRange :: (Eq a, Eq k, KDKey k) => Double -> k -> KDMap k a -> [(k,a)]
allInRange distThreshold k =
  filter (\p -> pointDistSq k (fst p) < distThreshold^(2::Int)) .
  allInRangeAux distThreshold k


allInRangeAux :: (Eq a, Eq k, KDKey k) => Double -> k
              -> KDMap k a -> [(k,a)]
allInRangeAux _ _ KDEmpty = []
allInRangeAux distThreshold k' (KDLeaf k a d)
  | pointDDistSq k' k d <= distThreshold^(2::Int) = [(k,a)]
  | otherwise                                     = []
allInRangeAux distThreshold k' (KDBranch k a d treeL treeR)
  | pointDDistSq k' k d <= distThreshold^(2::Int) =
    [(k,a)]
    ++ allInRangeAux distThreshold k' treeL
    ++ allInRangeAux distThreshold k' treeR
  | otherwise = []

------------------------------------------------------------------------------
isValid :: (Eq k, KDKey k,Show a,Show k) => KDMap k a -> Bool
isValid KDEmpty = True
isValid (KDLeaf _ _ _) = True
isValid (KDBranch k _ d kdLeft kdRight) =
  thisValid && isValid kdLeft && isValid kdRight
  where thisValid = all (\(k',_) -> dimOrder k' k d == LT) (toList kdLeft)
                    &&
                    all (\(k',_) -> dimOrder k' k d /= LT) (toList kdRight)

fromListWithDepth :: (KDKey k) => Depth -> [(k,a)] -> KDMap k a
fromListWithDepth _ [] = KDEmpty
fromListWithDepth d [(k,a)] = KDLeaf k a d
fromListWithDepth d ps@((k,_):_) = node'
  where
    psSort = L.sortBy (comparing (flip pointD d . fst)) ps
    medInd = L.length psSort `div` 2
    (kMed,aMed) = psSort !! medInd
    kdLeft  = fromListWithDepth (dSucc k d) (take medInd psSort)
    kdRight = fromListWithDepth (dSucc k d) (drop (medInd + 1) psSort)
    node' = KDBranch kMed aMed d kdLeft kdRight

keys :: KDMap k a -> [k]
keys  KDEmpty                        = []
keys (KDLeaf k _ _ )                 = [k]
keys (KDBranch k _ _ kdLeft kdRight) = k : (keys kdLeft ++ keys kdRight)

insert :: (Eq k, KDKey k) => Depth -> k -> a -> KDMap k a -> KDMap k a
insert d k a KDEmpty = KDLeaf k a d
insert _ k a (KDLeaf k' a' d')
  | k == k' = KDLeaf k a d'
  | otherwise = case dimOrder k k' d' of
    LT -> KDBranch k' a' d' (KDLeaf k a (dSucc k d')) KDEmpty
    _  -> KDBranch k' a' d' KDEmpty (KDLeaf k a (dSucc k d'))
insert _ k a (KDBranch k' a' d' kdLeft kdRight)
  | k == k' = KDBranch k a d' kdLeft kdRight
  | otherwise = case dimOrder k k' d' of
    LT -> KDBranch k' a' d' (insert (dSucc k d') k a kdLeft) kdRight
    _  -> KDBranch k' a' d' kdLeft (insert (dSucc k d') k a kdRight)
