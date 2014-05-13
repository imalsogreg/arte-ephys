module Arte.ArteDecode.KdMap where

import Data.Vector.Unboxed hiding (fromList)
import qualified Data.Vector.Unboxed as U
import qualified Data.List as L

-- Functions on Points
type Point a v = (Vector Double,v)

pointFromList :: [Double] -> v -> Point v
pointFromList xs a = (U.fromList xs, a)

nDimension :: (a -> Point v) -> a -> Int
nDimension f (p,_) = U.length (f p)

unsafeElement :: (a -> Point v) -> Int -> a -> Double
unsafeElement f d (p,_) = (U.!) p d

element :: Int -> Point a -> Maybe Double
element d (p,_) = (U.!?) p d

diff2 :: Int -> Point a -> Point a -> Double
diff2 d a b
  | d < nDimension a && d < nDimension b =
    (unsafeElement d a - unsafeElement d b) ^ (2::Int)
  | otherwise = 0

dist2 :: Point a -> Point b -> Double
dist2 (a,_) (b,_)  = U.sum $ U.zipWith (\x0 x1 -> (x1-x0)^(2::Int)) a b

-- used anywhere?
compareDistance :: Point a -> Point a -> Point a -> Ordering
compareDistance a b c =
  (dist2 a b) `compare` (dist2 b c)


data KdMap a = KdNode { kdLeft  :: KdMap a
                      , kdPoint :: Point a
                      , kdRight :: KdMap a
                      , kdAxis  :: Int
                      } 
             | KdEmpty
             deriving (Eq, Ord, Show)

mapFromList :: [Point a] -> KdMap a
mapFromList ps = fromListWithDepth ps 0

fromListWithDepth :: [Point a] -> Int -> KdMap a
fromListWithDepth [] _ = KdEmpty
fromListWithDepth ps depth = node
  where axis = depth `mod` nDimension (Prelude.head ps)

        -- Sort point list and choose median as pivod element
        sortedPoints =
          L.sortBy (\a b -> unsafeElement axis a
                            `compare` unsafeElement axis b) ps
        medianIndex = Prelude.length sortedPoints `div` 2

        -- Create node and construct subtrees
        node = KdNode { kdLeft = fromListWithDepth
                                 (Prelude.take medianIndex sortedPoints)
                                 (depth + 1)
                      , kdPoint = sortedPoints !! medianIndex
                      , kdRight = fromListWithDepth
                                  (Prelude.drop (medianIndex + 1)
                                   sortedPoints) (depth+1)
                      , kdAxis = axis }

addPoint :: Point a -> KdMap a -> KdMap a
addPoint point@(p,a) m = 