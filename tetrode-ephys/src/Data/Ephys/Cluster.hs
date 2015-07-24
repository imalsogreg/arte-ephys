{-|
Module      : Data.Ephys.Cluster
Description : Cluster boundaries
Copyright   : (c) 2015 Greg Hale, Shea Levy
License     : BSD3
Maintainer  : imalsogreg@gmail.com
Stability   : experimental
Portability : GHC, Linux
-}
{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module Data.Ephys.Cluster where

import Data.Ephys.Spike
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Vector.Unboxed as U
import Data.Serialize
import GHC.Generics (Generic)

import Control.Lens

-- | Channel index
type ChanInd = Int
-- | List of polygon points in trode voltage by trode voltage space, in Volts
type Polygon = [(Double,Double)]

-- | A cartesian boundary, defined by a pair of channels and a polygon in the
--   projection of the total voltage space onto the plane they span
data CartBound = CartBound { _cartXChan   :: !ChanInd -- ^ The X channel
                           , _cartYChan   :: !ChanInd -- ^ The Y channel
                           , _cartPolygon :: !Polygon -- ^ The bounding polygon
                           }
                      deriving (Eq, Show,Generic)

$(makeLenses ''CartBound)

instance Serialize CartBound where

-- | A method of clustering
data ClusterMethod =
  ClustCartBound       !CartBound -- ^ A single cartesian boundary
    -- | The intersection of multiple boundaries
  | ClustIntersection  ![ClusterMethod]
    -- | The union of multiple boundaries
  | ClustUnion         ![ClusterMethod]
  deriving (Eq, Show,Generic)

instance Serialize ClusterMethod where

-- | Determine if a spike is in a cluster
spikeInCluster :: ClusterMethod -- ^ The method of clustering
               -> TrodeSpike -- ^ The spike of interest
               -> Bool
spikeInCluster (ClustCartBound cb) s =
  pointInPolygon (cb ^. cartPolygon) p
  where
    amps = spikeAmplitudes s :: V.Vector Double
    p    = (amps ! (cb ^. cartXChan), amps ! (cb ^. cartYChan))
spikeInCluster (ClustIntersection cbs) s =
  all (`spikeInCluster` s) cbs
spikeInCluster (ClustUnion cbs) s =
  any (`spikeInCluster` s) cbs

  

{- Don't Delete 
-- Transcribe this very imparative c code
int pnpoly(int nvert, float *vertx, float *verty, float testx, float testy)
{
  int i, j, c = 0;
  for (i = 0, j = nvert-1; i < nvert; j = i++) {
    if ( ((verty[i]>testy) != (verty[j]>testy)) &&
     (testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
       c = !c;
  }
  return c;
}
-}

-- | Determine if a point is in a polygon
pointInPolygon :: Polygon -- ^ The polygon of interest
               -> (Double,Double) -- ^ The point of interest
               -> Bool
pointInPolygon polyPts' (tx,ty) = 
  loop 0 lastInd False where
    lastInd = length polyPts' - 1
    (xs,ys) = let pts = U.fromList polyPts' in U.unzip pts  
    (xn,yn) = ((xs U.!), (ys U.!))  -- convenient accessors
    loop i j c
      | i == length polyPts' = c --return
      | yTest && mTest       = loop (succ i) i (not c)
      | otherwise            = loop (succ i) i c
      where
        yTest     = (yn i > ty) /= (yn j > ty)
        -- slope test
        mTest = tx < (xn j - xn i)*(ty - yn i)/(yn j - yn i) + xn i
