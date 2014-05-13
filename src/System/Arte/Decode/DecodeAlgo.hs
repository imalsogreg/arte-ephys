{-# LANGUAGE BangPatterns #-}

module System.Arte.Decode.DecodeAlgo where

import System.Arte.Decode.DecoderDefs
import System.Arte.Decode.DecoderState
import Data.Ephys.EphysDefs
--import Data.Ephys.Spike
import Data.Ephys.PlaceCell
import Data.Ephys.TrackPosition

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Map.Strict as Map
import Data.Map.Strict (unionWith,unionsWith)
import Data.Ord
--import Control.Monad
--import qualified Data.Foldable as F
import Control.Concurrent.STM
import qualified Data.List as L
import Control.Lens
import Control.Applicative
import Data.Time.Clock
import System.IO

pcFieldRate :: Field Double -> Field Double -> Field Double
pcFieldRate occ field = Map.unionWith (/) field occ

type TrodeCollection a = Map.Map TrodeName (Map.Map PlaceCellName a)

stepReconstruction :: Double ->
                      TVar DecoderState -> Maybe Handle -> 
                      IO ()
stepReconstruction rTauSec dsT h = do
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

gt0 :: Field Double -> Field Double
gt0 = Map.map (\n -> if n > 0 then n else 0.1)

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

{-
liftTC2 :: (a -> a -> a) -> TrodeCollection a -> TrodeCollection a -> TrodeCollection a
liftTC2 = unionWith
-}

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
