{-# LANGUAGE TemplateHaskell #-}

-- A one-off histogram module for performance tracking

module System.Arte.Decode.Histogram where

import           Control.Lens
import           Control.Concurrent.STM
import           Data.Time
import qualified Data.Vector  as V

data Histogram a = Histogram {
    _bins   :: V.Vector a
  , _counts :: V.Vector Int
  }
                   
$(makeLenses ''Histogram)


------------------------------------------------------------------------------
timeAction :: TVar (Histogram Double) -> IO a -> IO a
timeAction h action = do
  t0   <- getCurrentTime
  a <- action
  tNow <- getCurrentTime
  atomically $ modifyTVar h (flip insert (realToFrac $ diffUTCTime tNow t0))
  return a


------------------------------------------------------------------------------
edgesToCenters :: Fractional a => V.Vector a -> V.Vector a
edgesToCenters edges = V.zipWith (\a b -> (a+b)/2) edges (V.tail edges)


------------------------------------------------------------------------------
insert :: RealFrac a => Histogram a -> a -> Histogram a
insert h x = case sampleBin h x of
  Nothing -> h
  Just i  -> let v = (h^.counts) V.! i         -- TODO, isn't there a cleaner
             in h & counts %~ (V.// [(i,v+1)]) -- way with lens?


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
     else Just $ min
          (V.length (h^.counts) - 1) (floor floatIndex)


