{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module System.Arte.Decode.Algorithm where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import qualified Data.List                as L
import qualified Data.Map.Strict          as Map
import           Data.Time.Clock
import qualified Data.Vector.Unboxed      as U
import qualified Data.Vector              as V
import           System.IO
------------------------------------------------------------------------------
import           Data.Ephys.EphysDefs
import           Data.Map.KDMap
import qualified Data.Map.KDMap                   as KDMap
import           Data.Ephys.PlaceCell
import           Data.Ephys.TrackPosition
import qualified System.Arte.Decode.Histogram    as H
import           System.Arte.Decode.Types
import           System.Arte.Decode.Config


------------------------------------------------------------------------------
pcFieldRate :: Field -> Field -> Field
pcFieldRate occ field = V.zipWith (/) field occ


------------------------------------------------------------------------------
type TrodeCollection a = Map.Map TrodeName (Map.Map PlaceCellName a)


------------------------------------------------------------------------------
runClusterReconstruction :: Double ->
                      TVar DecoderState -> Maybe Handle -> 
                      IO ()
runClusterReconstruction rTauSec dsT h = do
--  print "RUN CLUSTER RECONSTRUCTION"
  ds <- readTVarIO $ dsT
  let occT = ds ^. occupancy
      Clustered clusteredTrodes = ds^.trodes
      go lastFields = do
        t0 <- getCurrentTime
        delay <- async $ threadDelay (floor $ rTauSec * 1000000)
        (fields,counts) <- unzip <$> clusteredUnTVar clusteredTrodes
        occ <- readTVarIO occT
        let !estimate = clusteredReconstruction rTauSec lastFields counts occ
        atomically $ writeTVar (ds^.decodedPos) estimate
        resetClusteredSpikeCounts clusteredTrodes
        tNow <- getCurrentTime
        maybe (return ()) (flip hPutStrLn (showPosterior estimate tNow)) h
        atomically $ modifyTVar (ds^.decodeProf)
          (flip H.insert (realToFrac $ diffUTCTime t0 tNow))
        wait delay
        go fields
      fields0 = [] -- TODO: fix. (locks up if clusteredReconstrution doesn't
                   --             check for exactly this case)
    in
   go fields0


------------------------------------------------------------------------------
-- P(x|n) = C(tau,N) * P(x) * Prod_i(f_i(x) ^ n_i) * exp (-tau * Sum_i( f_i(x) ))
--                                                               |sumFields|
clusteredReconstruction :: Double -> [Field] -> [Int] -> Field -> Field
clusteredReconstruction _        []            _           occ   =
  V.replicate (V.length occ) (1 / (fromIntegral $ V.length occ))
clusteredReconstruction rTauSecs clusterFields clusterCounts occ =
  let clusterFieldsGt0 = map gt0 clusterFields :: [Field]
      sumFields      = V.zipWith (/)
                       (L.foldl1' (V.zipWith (+)) clusterFieldsGt0)
                       occ
      bayesField f c = V.map (^c) (V.zipWith (/) f occ) :: Field
      prodPart       = L.foldl1' (V.zipWith (*))
                       (zipWith bayesField clusterFieldsGt0 clusterCounts)
      exponPart      = V.map (exp . (* negate rTauSecs)) sumFields
      likelihoodPart = V.zipWith (*) prodPart exponPart
      posteriorPart  = V.zipWith (*) occ likelihoodPart :: Field
      invPostSum     = 1 / V.sum  posteriorPart
  in
   V.map (* invPostSum) posteriorPart 


------------------------------------------------------------------------------
gt0 :: Field -> Field
gt0 = V.map (\x -> if x > 0 then x else 0.1)


------------------------------------------------------------------------------
normalize :: Field -> Field
normalize f = let fieldSum = V.foldl' (+) 0 f
                  coef = 1/fieldSum
              in  V.map (*coef) f
{-# INLINE normalize #-}

------------------------------------------------------------------------------
clusteredUnTVar :: Map.Map PlaceCellName PlaceCellTrode -> IO [(Field,Int)]
clusteredUnTVar pcMap = fmap concat $ atomically . mapM trodeFields . Map.elems $ pcMap
  where
    trodeFields :: PlaceCellTrode -> STM [(Field,Int)]
    trodeFields pct = mapM countsOneCell
                      (Map.elems . _dUnits $ pct)
    countsOneCell :: TVar DecodablePlaceCell -> STM (Field, Int)
    countsOneCell dpcT = do
      dpc <- readTVar dpcT
      return (dpc^.dpCell.countField, dpc^.dpCellTauN)


------------------------------------------------------------------------------
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
posteriorOut :: Field -> [Double]
posteriorOut f = V.toList f


------------------------------------------------------------------------------
showPosterior :: Field -> UTCTime -> String
showPosterior f (UTCTime _ sec) =
  (take 10 $ show sec) ++ ", " ++ L.intercalate ", " (map show $ posteriorOut f)


------------------------------------------------------------------------------
runClusterlessReconstruction :: ClusterlessOpts -> Double -> TVar DecoderState
                             -> Maybe Handle -> IO ()
runClusterlessReconstruction rOpts rTauSec dsT h = readTVarIO dsT >>= go
  where go ds = do
          t0 <- getCurrentTime
          timer  <- async $ threadDelay (floor $ rTauSec * 1e6)
          do
            ds <- readTVarIO dsT
            !trodeEstimates <- forM
                              (Map.elems $ ds^.trodes._Clusterless) $
                              (stepTrode rOpts)
            let !fieldProduct = collectFields trodeEstimates
            atomically . writeTVar (ds^.decodedPos) .  normalize $ fieldProduct
          putStrLn ""
          tNow <- getCurrentTime
          atomically $ modifyTVar (ds^.decodeProf)
            (flip H.insert (realToFrac $ diffUTCTime t0 tNow))
          wait timer
          go ds
  

------------------------------------------------------------------------------
stepTrode :: ClusterlessOpts -> TVar ClusterlessTrode -> IO Field
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

  putStr $ show (length $ filter okAmp spikes) ++ "/" ++
    show (length spikes) ++ " spikes. " 
  return . collectFields $ 
    map (\s -> sampleKDE opts s kde) (filter okAmp spikes)
  where fst3 (a,_,_) = a
        snd3 (_,b,_) = b
        trd3 (_,_,c) = c
        okAmp  p     = U.maximum (_pAmplitude p) >= amplitudeThreshold opts


------------------------------------------------------------------------------
collectFields :: [Field] -> Field
collectFields = normalize . V.map (exp)
                . L.foldl' (V.zipWith (+)) zerosField
                . map (V.map log)


------------------------------------------------------------------------------
sampleKDE :: ClusterlessOpts -> ClusterlessPoint -> NotClust -> Field
sampleKDE ClusterlessOpts{..} point points =
  let nearbyPoints   = map fst $ allInRange (sqrt cutoffDist2) point points :: [ClusterlessPoint]
      distExponent :: ClusterlessPoint -> Double
      distExponent p = (1 / ) $
                       exp((-1) * (pointDistSq p point :: Double)/(2*kernelVariance))
      expField :: ClusterlessPoint -> Field
      expField  p    = V.map (** distExponent p) (_pField p)
--      expField  p    = _pField p
--      scaledField :: Field -> Field
--      scaledField p  = V.map (/ (V.sum p)) p
--  in bound 0.1 1000 $
--     L.foldl' (V.zipWith (*)) emptyField $ map (normalize . bound 0.1 1000 . expField) nearbyPoints
  in normalize . collectFields . map expField $ nearbyPoints


------------------------------------------------------------------------------
closenessFromSample :: ClusterlessOpts -> ClusterlessPoint -> ClusterlessPoint
                       -> Double
closenessFromSample ClusterlessOpts{..} pointA pointB =
  exp((-1) * ((pointDistSq pointA pointB)/(2 *kernelVariance)))

------------------------------------------------------------------------------
data ClusterlessOpts = ClusterlessOpts {
    kernelVariance     :: !Double
  , cutoffDist2        :: !Double
  , amplitudeThreshold :: !Voltage
  , kdClumpThreshold   :: !Double
  } deriving (Eq, Show)

defaultClusterlessOpts :: ClusterlessOpts
defaultClusterlessOpts =  ClusterlessOpts (200e-6) ((50e-6)^2) (20e-6) (10e-6)



------------------------------------------------------------------------------
unZero :: Double -> Field -> Field
unZero baseline f = V.map (max baseline) f

unInf :: Double -> Field -> Field
unInf ceil f = V.map (min ceil) f

bound :: Double -> Double -> Field -> Field
bound l h = unInf h . unZero l
