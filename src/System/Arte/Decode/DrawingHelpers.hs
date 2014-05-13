module System.Arte.Decode.DrawingHelpers where

import System.Arte.Decode.DecoderDefs
import Data.Ephys.EphysDefs

import Control.Lens
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM.TVar
import qualified Data.CircularList as CL
import Graphics.Gloss.Interface.IO.Game


{- New way, hold TVars of DecodablePlaceCells in the draw opt -}
data TrodeDrawOption = DrawPlaceCell   PlaceCellName (TVar DecodablePlaceCell)
                     | DrawClusterless (TVar ClusterlessTrode)
                     | DrawOccupancy
                     | DrawDecoding
                     | DrawError String
                     deriving (Eq)

instance Show TrodeDrawOption where
  show (DrawPlaceCell name _)   = "DrawPlaceCell " ++ show name
  show (DrawClusterless _) = "DrawClusterless"
  show  DrawOccupancy      = "DrawOccupancy"
  show  DrawDecoding       = "DrawDecoding"
  show (DrawError s)       = "DrawError " ++ s

type TrodeDrawOptions = CL.CList (CL.CList TrodeDrawOption)

drawDrawOptionsState :: TrodeDrawOptions -> TrodeDrawOption -> Picture
drawDrawOptionsState opts opt =
  pictures $
  zipWith (drawOptsFamily opt) (CL.toList opts) [0..CL.size opts - 1]
  where drawOptsFamily targetOpt subOpts ind =
          pictures $
          zipWith (drawOpt targetOpt ind)
          (CL.toList subOpts) [0..CL.size subOpts - 1]
        drawOpt targetOpt ind thisOpt subInd
          | thisOpt == targetOpt = Color blue
                                   (Translate (fi ind) (fi subInd) (Circle 0.5))
          | otherwise            = Color blue
                                   (Translate (fi ind) (fi subInd) (ThickCircle 0.2 0.5))
        fi = fromIntegral
          
clistTrodes :: Trodes -> TrodeDrawOptions
clistTrodes (Clustered tMap) =
  CL.fromList $ map clistTrode (Map.elems tMap) ++
  [CL.singleton DrawOccupancy, CL.singleton DrawDecoding]
    where
      clistTrode :: (PlaceCellTrode) -> CL.CList TrodeDrawOption
      clistTrode (PlaceCellTrode units _) =
        CL.fromList $ map (\(n,u) -> DrawPlaceCell n u)
        (Map.toList units)
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
  where
    subOpt :: (CL.CList TrodeDrawOption -> CL.CList TrodeDrawOption) -> CL.CList TrodeDrawOption
    subOpt rotDir = case CL.focus opt of
          Nothing -> error "DrawingHelpers: While rotating DrapOpt, found Empty CList, unexpected"
          Just f  -> rotDir f
