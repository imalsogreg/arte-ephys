{-# LANGUAGE BangPatterns #-}

module DecodeAlgo where

import DecoderDefs
import DecoderState
import Data.Ephys.EphysDefs
import Data.Ephys.Spike
import Data.Ephys.PlaceCell
import Data.Ephys.TrackPosition

import qualified Data.Map.Strict as Map
import Control.Monad
import qualified Data.Foldable as F
import Control.Concurrent.STM

import Control.Lens
import Control.Applicative

pcFieldRate :: Field Double -> Field Double -> Field Double
pcFieldRate occ field = Map.unionWith (/) field occ

type TrodeCollection a = Map.Map TrodeName (Map.Map PlaceCellName a)

stepReconstruction :: Double ->
                      TrodeCollection (Field Double) ->
                      TrodeCollection (TVar PlaceCellTrode) ->
                      IO (TrodeCollection (Field Double), Field Double)
stepReconstruction lastFields clusteredTrodes = do
  thisFieldsCounts <- clusteredCounts clusteredTrodes
  undefined

-- P(x|n) = C(tau,N) * P(x) * Prod_i(f_i(x) ^ n_i) * exp (-tau * Sum_i( f_i(x) ))
clusteredReconstruction :: Map.Map TrodeName PlaceCellTrode -> Field Double
clusteredReconstruction clusteredTrodes = undefined

clusteredCounts :: Map.Map PlaceCellName PlaceCellTrode -> IO [(Field Double,Int)]
clusteredCounts pcMap = fmap concat $ atomically . mapM trodeFields . Map.elems $ pcMap
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

unTVarClustered :: (TrodeCollection PlaceCellTrode) -> TrodeCollection (Field Double, Int)
unTVarClustered = undefined

liftTC :: (a -> b) -> TrodeCollection a -> TrodeCollection b
liftTC f tca = Map.map (Map.map f) tca

{-
liftTC2 :: (a -> b -> c) -> TrodeCollection a -> TrodeCollection b -> TrodeCollection c
liftTC2 f tca tcb = (liftA2 . liftA2) f tca tcb 
-}
