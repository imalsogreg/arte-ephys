{-# LANGUAGE TemplateHaskell #-}

-- A one-off histogram module for performance tracking

module System.Arte.Decode.Histogram where

import           Control.Lens
import qualified Data.Vector  as V

data Histogram a = Histogram {
    _bins   :: V.Vector a
  , _counts :: V.Vector Int
  }
                   
$(makeLenses ''Histogram)

------------------------------------------------------------------------------
edgesToCenters :: Fractional a => V.Vector a -> V.Vector a
edgesToCenters edges = V.zipWith (\a b -> (a+b)/2) edges (V.tail edges)


------------------------------------------------------------------------------
mkHistogram :: RealFrac a => (a,a) -> Int -> Histogram a
mkHistogram (firstStart, lastStop) nBins =
  let dBin = (lastStop - firstStart) / fromIntegral nBins
      bs   = V.generate nBins ((+ firstStart) . (*dBin) . fromIntegral)
      cs   = V.replicate nBins 0
  in  Histogram bs cs

      
------------------------------------------------------------------------------
defaultHistogram :: Histogram Double
defaultHistogram = mkHistogram (0,0.1) 100

------------------------------------------------------------------------------
sampleBin :: (RealFrac a) => Histogram a -> a -> Maybe Int
sampleBin h x =
  let dBin       = ((h^.bins) V.! 1) - ((h^.bins) V.! 0)
      floatIndex = (x - (h^.bins) V.! 0) / dBin
  in if   floatIndex < 0
     then Nothing
     else Just $ max
          (V.length (h^.counts) - 1) (floor floatIndex)


