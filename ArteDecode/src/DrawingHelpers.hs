module DrawingHelpers where

import DecoderDefs
import Data.Ephys.EphysDefs

import Control.Lens
import qualified Data.Map as Map
import Control.Concurrent.STM
import qualified Data.CircularList as CL
import Graphics.Gloss.Interface.IO.Game


{- Old way, based on nested maps -}
{-data TrodeDrawOption = DrawPlaceCell TrodeName PlaceCellName
                     | DrawClusterless TrodeName
                     | DrawOccupancy
                     | DrawDecoding
                     | DrawError String
-}

{- New way, hold TVars of DecodablePlaceCells in the draw opt -}
data TrodeDrawOption = DrawPlaceCell   (TVar DecodablePlaceCell)
                     | DrawClusterless (TVar ClusterlessTrode)
                     | DrawOccupancy
                     | DrawDecoding
                     | DrawError String

instance Show TrodeDrawOption where
  show (DrawPlaceCell _)   = "DrawPlaceCell"
  show (DrawClusterless _) = "DrawClusterless"
  show  DrawOccupancy      = "DrawOccupancy"
  show  DrawDecoding       = "DrawDecoding"
  show (DrawError s)       = "DrawError " ++ s
 
type TrodeDrawOptions = CL.CList (CL.CList TrodeDrawOption)

clistTrodes :: Trodes -> TrodeDrawOptions
clistTrodes (Clustered tMap) =
  CL.fromList $ map clistTrode (Map.elems tMap) ++
  [CL.singleton DrawOccupancy, CL.singleton DrawDecoding]
    where
      clistTrode :: (PlaceCellTrode) -> CL.CList TrodeDrawOption
      clistTrode (PlaceCellTrode units _) =
        CL.fromList (map DrawPlaceCell $ Map.elems units)
        -- [DrawPlaceCell tName cName | cName <- Map.keys (cMap^.dUnits)]  
clistTrodes (Clusterless tMap) =
  CL.fromList $ map (CL.singleton . DrawClusterless) (Map.elems tMap)

stepDrawOpt :: SpecialKey -> TrodeDrawOptions -> TrodeDrawOptions
stepDrawOpt k opt
  | k == KeyRight = CL.rotR opt
  | k == KeyLeft  = CL.rotL opt
  | k == KeyDown  = CL.update (subOpt CL.rotR) opt
  | k == KeyUp    = CL.update (subOpt CL.rotL) opt
  | otherwise     = opt
  where subOpt rot = case CL.focus opt of
          Nothing -> error "Empty CList, unexpected"
          Just f  -> rot f
