module System.Arte.Decode.DrawingHelpers where

------------------------------------------------------------------------------
import           Control.Lens
import qualified Data.CircularList as CL
import qualified Data.Map.Strict as Map
import           Graphics.Gloss.Interface.IO.Game
------------------------------------------------------------------------------
import           System.Arte.Decode.DecoderDefs
import           System.Arte.Decode.DecoderState


------------------------------------------------------------------------------
drawDrawOptionsState :: DecoderState -> Picture
drawDrawOptionsState ds =
  let filledCol = ds^.trodeInd
      filledRow = ds^.clustInd
      drawCol cInd col = pictures $
                         map (drawDot cInd col) [0..CL.size col - 1]
      drawDot        = \cInd c rInd ->
        translate (fromIntegral cInd) (fromIntegral rInd) $
        if filledCol `mod` nOpts     == cInd &&
           filledRow `mod` CL.size c == rInd :: Bool
        then color blue $ Circle 0.5 :: Picture
        else color red  $ ThickCircle 0.1 0.4 :: Picture
      nOpts          = length optsList
      optsList       = CL.toList $ ds^.trodeDrawOpt
  in  pictures $ zipWith drawCol [0..nOpts-1] optsList

        
------------------------------------------------------------------------------
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
