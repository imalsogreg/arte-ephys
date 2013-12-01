module DrawingHelpers where

import DecoderDefs
import Data.Ephys.EphysDefs

import qualified Data.Map as Map
import qualified Data.CircularList as CL
import Graphics.Gloss.Interface.IO.Game


data TrodeDrawOption = DrawPlaceCell TrodeName PlaceCellName
                     | DrawClusterless TrodeName
                     | DrawOccupancy
                     | DrawDecoding
                     | DrawError String

type TrodeDrawOptions = CL.CList (CL.CList TrodeDrawOption)

clistTrodes :: Trodes -> TrodeDrawOptions
clistTrodes (Clustered tMap) =
  CL.fromList $ map clistTrode (Map.toList tMap) ++
  [CL.singleton DrawOccupancy, CL.singleton DrawDecoding]
    where
      clistTrode :: (TrodeName,PlaceCellTrode) -> CL.CList TrodeDrawOption
      clistTrode (tName,cMap) =
        CL.fromList [DrawPlaceCell tName cName | cName <- Map.keys cMap]
clistTrodes (Clusterless tMap) =
  CL.fromList $ map clistTrode (Map.keys tMap)
    where
      clistTrode :: TrodeName -> CL.CList TrodeDrawOption
      clistTrode tName = CL.singleton (DrawClusterless tName)

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
