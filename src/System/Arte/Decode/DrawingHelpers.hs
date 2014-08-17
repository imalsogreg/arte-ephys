module System.Arte.Decode.DrawingHelpers where

------------------------------------------------------------------------------
import           Control.Concurrent.STM
import           Control.Lens
import qualified Data.CircularList                as CL
import qualified Data.Map.Strict                  as Map
import qualified Graphics.Gloss.Data.Color        as Color
import           Graphics.Gloss.Interface.IO.Game
------------------------------------------------------------------------------
import           Data.Map.KDMap
import           Data.Ephys.EphysDefs
import           Data.Ephys.GlossPictures
import           System.Arte.Decode.DecoderDefs
import           System.Arte.Decode.DecoderState


------------------------------------------------------------------------------
drawDrawOptionsState :: DecoderState -> Picture
drawDrawOptionsState ds =
  let filledCol = ds^.trodeInd
      filledRow = ds^.clustInd
      drawCol :: Int -> [TrodeDrawOption] -> Picture
      drawCol cInd col = pictures $
                         map (drawDot cInd col) [0..length col - 1]
      drawDot        = \cInd c rInd ->
        translate (fromIntegral cInd) (fromIntegral rInd) $
        if filledCol `mod` nOpts     == cInd &&
           filledRow `mod` length c == rInd :: Bool
        then color blue $ Circle 0.5 :: Picture
        else color red  $ ThickCircle 0.1 0.4 :: Picture
      nOpts          = length optsList
      optsList       = ds^.trodeDrawOpt
  in  pictures $ zipWith drawCol [0..nOpts-1] optsList


------------------------------------------------------------------------------
clistTrodes :: Trodes -> TrodeDrawOptions
clistTrodes (Clustered tMap) =
  (map f $ Map.toList tMap) ++
  [[DrawOccupancy], [DrawDecoding]]
    where
      f :: (TrodeName, PlaceCellTrode) -> [TrodeDrawOption]
      f (tName, PlaceCellTrode units _) = map (\(n,u) -> DrawPlaceCell n u)
                                          (Map.toList units)
clistTrodes (Clusterless tMap) =
  (map f $ Map.toList tMap)
  ++ [[DrawOccupancy], [DrawDecoding] ]
  where
    f :: (TrodeName,TVar ClusterlessTrode) -> [TrodeDrawOption]
    f (n,t) = [ DrawClusterless n t
                (ClessDraw (XChan x) (YChan y))
              | x <- [0  ..3]
              , y <- [x+1..3]
              ]

treeScale :: Float
treeScale = 2e6

treeTranslate :: Float
treeTranslate = (-200)


------------------------------------------------------------------------------
screenToTree :: (Float, Float) -> (Float, Float)
screenToTree (x,y) =
  ((x-treeTranslate)/treeScale, (y-treeTranslate)/treeScale)


------------------------------------------------------------------------------
treeToScreen :: (Float,Float) -> (Float, Float)
treeToScreen (x,y) =
  (x * treeScale + treeTranslate, y * treeScale + treeTranslate)


------------------------------------------------------------------------------
drawClusterlessPoint
  :: XChan -> YChan -> Double -> (ClusterlessPoint, MostRecentTime) -> Picture
drawClusterlessPoint (XChan x) (YChan y) tNow (cP,t) =
  translate (r2 $ pointD cP (Depth x)) (r2 $ pointD cP (Depth y)) $
  Pictures [ color (pointColor tNow t) $
             circleSolid (1e-6 + log (realToFrac $ pointW cP) / treeScale)
           , circle (1e-6 + log (r2 $ pointW cP) / treeScale)
           ]


------------------------------------------------------------------------------
drawTree
  :: XChan -> YChan -> Double -> KDMap ClusterlessPoint MostRecentTime -> Picture
drawTree xC yC tNow =
  Pictures . map (drawClusterlessPoint xC yC tNow) . toList


------------------------------------------------------------------------------
pointColor :: Double -> MostRecentTime -> Color.Color
pointColor tNow (MostRecentTime t) = Color.makeColor r g b 1
  where e tau = exp (-1 * (max dt 0) / tau)
        dt = (r2 tNow) - (r2 t)
        r  = e 0.2
        g  = e 1
        b  = e 20
