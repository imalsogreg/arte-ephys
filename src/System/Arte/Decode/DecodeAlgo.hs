{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module System.Arte.Decode.DecodeAlgo where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import qualified Data.List                as L
import qualified Data.Map.Strict          as Map
import           Data.Map.Strict          (unionWith,unionsWith)
import           Data.Ord
import           Data.Time.Clock
import qualified Data.Vector.Unboxed      as U
import           System.IO
------------------------------------------------------------------------------
import           System.Arte.Decode.DecoderDefs
import           System.Arte.Decode.DecoderState
import           Data.Ephys.EphysDefs
import           Data.Map.KDMap
import qualified Data.Map.KDMap                   as KDMap
import           Data.Ephys.PlaceCell
import           Data.Ephys.TrackPosition


------------------------------------------------------------------------------
pcFieldRate :: Field Double -> Field Double -> Field Double
pcFieldRate occ field = Map.unionWith (/) field occ


------------------------------------------------------------------------------
type TrodeCollection a = Map.Map TrodeName (Map.Map PlaceCellName a)


------------------------------------------------------------------------------
runClusterReconstruction :: Double ->
                      TVar DecoderState -> Maybe Handle -> 
                      IO ()
runClusterReconstruction rTauSec dsT h = do
  ds <- readTVarIO $ dsT
  let occT = ds ^. occupancy
      Clustered clusteredTrodes = ds^.trodes
      go lastFields = do
        delay <- async $ threadDelay (floor $ rTauSec * 1000000)
        (fields,counts) <- unzip <$> clusteredUnTVar clusteredTrodes
        occ <- readTVarIO occT
        let !estimate = clusteredReconstruction rTauSec lastFields counts occ
        atomically $ writeTVar (ds^.decodedPos) estimate
        resetClusteredSpikeCounts clusteredTrodes
        tNow <- getCurrentTime
        maybe (return ()) (flip hPutStrLn (showPosterior estimate tNow)) h
        wait delay
        go fields
      fields0 = [] -- Will this work?
    in
   go fields0


------------------------------------------------------------------------------
-- P(x|n) = C(tau,N) * P(x) * Prod_i(f_i(x) ^ n_i) * exp (-tau * Sum_i( f_i(x) ))
--                                                               |sumFields|
clusteredReconstruction :: Double -> [Field Double] -> [Int] -> Field Double -> Field Double
clusteredReconstruction rTauSecs clusterFields clusterCounts occ =
  let clusterFieldsGt0 = map gt0 clusterFields
      sumFields      = unionWith (/) (unionsWith (+) clusterFieldsGt0) occ :: Field Double
      bayesField f c = Map.map (^c) (unionWith (/) f occ)
      prodPart       = unionsWith (*) (zipWith bayesField clusterFieldsGt0 clusterCounts)
      exponPart      = Map.map (exp . (* negate rTauSecs)) sumFields
      likelihoodPart = unionWith (*) prodPart exponPart
      posteriorPart  = unionWith (*) occ likelihoodPart :: Field Double
      posteriorSum   = sum . Map.elems $ posteriorPart
  in
   Map.map (/posteriorSum) posteriorPart 


------------------------------------------------------------------------------
gt0 :: Field Double -> Field Double
gt0 = Map.map (\n -> if n > 0 then n else 0.1)


------------------------------------------------------------------------------
normalize :: Field Double -> Field Double
normalize f = let fieldSum = L.foldl' (+) 0 (Map.elems f)
                  coef = 1/ max 0.1 fieldSum
              in  Map.map (*coef) f

------------------------------------------------------------------------------
clusteredUnTVar :: Map.Map PlaceCellName PlaceCellTrode -> IO [(Field Double,Int)]
clusteredUnTVar pcMap = fmap concat $ atomically . mapM trodeFields . Map.elems $ pcMap
  where
    trodeFields :: PlaceCellTrode -> STM [(Field Double,Int)]
    trodeFields pct = mapM countsOneCell
                      (Map.elems . _dUnits $ pct)
    countsOneCell :: TVar DecodablePlaceCell -> STM (Field Double, Int)
    countsOneCell dpcT = do
      dpc <- readTVar dpcT
      return (dpc^.dpCell.countField, dpc^.dpCellTauN)

resetClusteredSpikeCounts :: Map.Map TrodeName PlaceCellTrode
                    -> IO ()
resetClusteredSpikeCounts clusteredTrodes = 
  atomically $ mapM_ (\dpc -> resetOneTrode dpc) (Map.elems clusteredTrodes)
  where resetOneTrode t = mapM_ resetOneCell (Map.elems . _dUnits $ t)
        resetOneCell pc = modifyTVar pc $ dpCellTauN .~ 0

liftTC :: (a -> b) -> TrodeCollection a -> TrodeCollection b
liftTC f tca = Map.map (Map.map f) tca


------------------------------------------------------------------------------
keyFilter :: (Ord k) => (k -> Bool) -> Map.Map k a -> Map.Map k a
keyFilter p m = Map.filterWithKey (\k _ -> p k) m

------------------------------------------------------------------------------
posteriorOut :: Field Double -> [Double]
posteriorOut f =
  map snd
  . filter ( ((==Outbound)._trackDir) . fst)
  . filter ( ((==InBounds)._trackEcc) . fst)
  . L.sortBy (comparing (_binName . _trackBin . fst))
  . Map.toList
  $ f  

showPosterior :: Field Double -> UTCTime -> String
showPosterior f (UTCTime _ sec) =
  (take 10 $ show sec) ++ ", " ++ L.intercalate ", " (map show $ posteriorOut f)



------------------------------------------------------------------------------
runClusterlessReconstruction :: ClusterlessOpts -> Double -> TVar DecoderState
                             -> Maybe Handle -> IO ()
runClusterlessReconstruction rOpts rTauSec dsT h = go
  where go = do
          timer  <- async $ threadDelay (floor $ rTauSec * 1e6)
          do
            ds <- readTVarIO dsT
            trodeEstimatesA <- forM
                               (Map.elems $ ds^.trodes._Clusterless) $
                               (stepTrode rOpts)
--            trodeEstimates <- mapM wait trodeEstimatesA
            let fieldProduct = unionsWith (*) trodeEstimatesA
            atomically . writeTVar (ds^.decodedPos) $ normalize fieldProduct
          wait timer
          go
  

------------------------------------------------------------------------------
stepTrode :: ClusterlessOpts -> TVar ClusterlessTrode -> IO (Field Double)
stepTrode opts trode' = do
  (spikes,kde) <- atomically $ do
    trode <- readTVar trode'
    let spikesTimes = (trode^.dtTauN)
    let kde = (trode^.dtNotClust)
        f m k = KDMap.add m (kdClumpThreshold opts) (fst3 k) (snd3 k)
        spikesForField  = filter trd3 spikesTimes
    writeTVar trode' $
      ClusterlessTrode (L.foldl' f  kde spikesForField) []
    return $ (map fst3 spikesTimes,kde)
  return . unionsWith (*) $
    map (\s -> sampleKDE opts s kde) (filter amp spikes)
  where fst3 (a,_,_) = a
        snd3 (_,b,_) = b
        trd3 (_,_,c) = c
        amp  p       = U.maximum (pAmplitude p) >= amplitudeThreshold opts

------------------------------------------------------------------------------
sampleKDE :: ClusterlessOpts -> ClusterlessPoint -> NotClust -> Field Double
sampleKDE ClusterlessOpts{..} point points =
  let nearbyPoints   = map fst $ allInRange (sqrt cutoffDist2) point points
      distExponent p = (1 / ) $ exp((-1) * (pointDistSq p point)/
                                           (2*kernelVariance))
      expField  p    = Map.map (** distExponent p) (pField p)
      scaledField p  = Map.map (/ (sum $ Map.elems p)) p
  in unionsWith (*) $ map (scaledField . expField) nearbyPoints


------------------------------------------------------------------------------
data ClusterlessOpts = ClusterlessOpts {
    kernelVariance     :: !Double
  , cutoffDist2        :: !Double
  , amplitudeThreshold :: !Voltage
  , kdClumpThreshold   :: !Double
  } deriving (Eq, Show)

defaultClusterlessOpts :: ClusterlessOpts
defaultClusterlessOpts =  ClusterlessOpts 200e-6 5000e-6 100e-6 0.0002