module DrawingHelpers where

import DecoderDefs
import Data.Ephys.EphysDefs

import Control.Lens
import qualified Data.Map as Map
import Control.Concurrent.MVar
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
data TrodeDrawOption = DrawPlaceCell   (MVar DecodablePlaceCell)
                     | DrawClusterless (MVar ClusterlessTrode)
                     | DrawOccupancy
                     | DrawDecoding
                     | DrawError String
                     deriving (Eq)

instance Show TrodeDrawOption where
  show (DrawPlaceCell _)   = "DrawPlaceCell"
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
