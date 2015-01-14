{-# LANGUAGE TemplateHaskell #-}

-- A one-off histogram module for performance tracking

module System.Arte.Decode.Histogram where

import           Control.Applicative
import           Control.Lens
import           Control.Concurrent.STM
import qualified Data.Array.MArray as M
import           Data.Time
import qualified Data.Vector  as V

data Histogram a = Histogram {
    _binEdges :: V.Vector a
  , _counts   :: TArray Int Int
  }
                   
$(makeLenses ''Histogram)

------------------------------------------------------------------------------
timeAction :: Histogram Double -> IO a -> IO a
timeAction h action = do
  t0   <- getCurrentTime
  a <- action
  tNow <- getCurrentTime
  atomically $ insert h (realToFrac $ diffUTCTime tNow t0)
  return a


------------------------------------------------------------------------------
edgesToCenters :: Fractional a => V.Vector a -> V.Vector a
edgesToCenters edges = V.zipWith (\a b -> (a+b)/2) edges (V.tail edges)



------------------------------------------------------------------------------
insert :: RealFrac a => Histogram a -> a -> STM ()
insert h x = case sampleBin h x of
  Nothing -> return ()
  Just i  -> do
    v <-  M.readArray (h^.counts) i
    M.writeArray (h^.counts) i (v+1)


------------------------------------------------------------------------------
mkHistogram :: (RealFrac a, Enum a) => (a,a) -> Int -> STM (Histogram a)
mkHistogram (firstStart, lastStop) nBins =
  let dBin = (lastStop - firstStart) / fromIntegral nBins
      hBinEdges = V.fromList $ take nBins [firstStart, firstStart+dBin ..]
      hCounts   = M.newArray (0,nBins) 0 :: STM (TArray Int Int)
  in  Histogram hBinEdges <$> hCounts

      
------------------------------------------------------------------------------
defaultHistogram :: STM (Histogram Double)
defaultHistogram = mkHistogram (0,0.1) 100


------------------------------------------------------------------------------
sampleBin :: (RealFrac a) => Histogram a -> a -> Maybe Int
sampleBin h x =
  let dBin       = ((h^.binEdges) V.! 1) - ((h^.binEdges) V.! 0)
      floatIndex = (x - (h^.binEdges) V.! 0) / dBin
  in if   floatIndex < 0
     then Nothing
     else Just $ min
          (V.length (h^.binEdges) - 1) (floor floatIndex)


