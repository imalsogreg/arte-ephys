{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module Data.Ephys.Cluster where

import Data.Ephys.Spike
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Vector.Unboxed as U
import Data.Serialize
import GHC.Generics (Generic)

import Control.Lens

type ChanInd = Int
type Polygon = [(Double,Double)]

data CartBound = CartBound { _cartXChan   :: !ChanInd
                           , _cartYChan   :: !ChanInd
                           , _cartPolygon :: !Polygon
                           }
                      deriving (Eq, Show,Generic)

$(makeLenses ''CartBound)

instance Serialize CartBound where

data PolarBound = PolarBound -- Placeholder
                  deriving (Eq, Show, Generic)

instance Serialize PolarBound where
                           
data ClusterMethod =
  ClustCartBound       !CartBound
  | ClustPolarBound    !PolarBound
  | ClustSoftCartesian 
  | ClustIntersection  ![ClusterMethod]
  | ClustUnion         ![ClusterMethod]
  deriving (Eq, Show,Generic)

instance Serialize ClusterMethod where

spikeInCluster :: ClusterMethod -> TrodeSpike -> Bool
spikeInCluster (ClustCartBound cb) s =
  pointInPolygon (cb ^. cartPolygon) p
  where
    amps = spikeAmplitudes s :: V.Vector Double
    p    = (amps ! (cb ^. cartXChan), amps ! (cb ^. cartYChan))
spikeInCluster (ClustPolarBound _) _ =
  error "Not implemented polar clusts"
spikeInCluster (ClustSoftCartesian) _ =
  error "Not implemented soft cartesian clusts"
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

pointInPolygon :: Polygon -> (Double,Double) -> Bool
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
