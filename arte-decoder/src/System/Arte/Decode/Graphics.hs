module System.Arte.Decode.Graphics where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.STM
import qualified Control.Concurrent.STM.TArray    as T
import           Control.Lens
import qualified Data.Array.MArray                as M
import qualified Data.CircularList                as CL
import qualified Data.Foldable                    as F
import qualified Data.List                        as L
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe, catMaybes)
import           Data.Time
import qualified Data.Vector                      as V
import qualified Graphics.Gloss.Data.Color        as Color
import           Graphics.Gloss.Interface.IO.Game
import           Safe                             (headMay)
------------------------------------------------------------------------------
import           Data.Map.KDMap
import           Data.Ephys.EphysDefs
import           Data.Ephys.TrackPosition
import           Data.Ephys.GlossPictures
import           System.Arte.Decode.Types
import           System.Arte.Decode.Config
import           System.Arte.Decode.Histogram
import           System.Arte.Decode.Algorithm

------------------------------------------------------------------------------
drawOptionsStatePic :: DecoderState -> Picture
drawOptionsStatePic ds =
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
mkClusterlessScreenPic :: TrodeDrawOption -> DecoderState -> IO Picture
mkClusterlessScreenPic (DrawClusterless tName kdT (ClessDraw x y)) ds =
  do
    tNow <- (ds^.toExpTime) <$> getCurrentTime
    kd   <- atomically $ readTVar kdT
    let treePic = treePicture x y tNow (kd^.dtNotClust)
        treeSelectedPic = (flip fmap) (ds^.samplePoint) $ \cp ->
          let closestP    = fromMaybe cp $ fst <$> closest cp (kd^.dtNotClust)
              psInRange   =  map fst $ allInRange
                             (sqrt $ cutoffDist2 defaultClusterlessOpts)
                             cp (kd^.dtNotClust)
              selectColor = Color.makeColor 0 1 0 0.5
              inRangeFld  = collectFields .
                            map (\r -> bound 0.01 0.9 $ r^.pField) .
                            take 100 $
                            psInRange
          in Pictures $
             [fieldPic inRangeFld
             ,clusterlessPointPic x y tNow (closestP,(MostRecentTime tNow))
             ,color (Color.makeColor 1 0 0 0.1) $
              pointAtSize x y cp (sqrt $ cutoffDist2 defaultClusterlessOpts)
             ]
             ++ map (\pt -> color selectColor $ pointAtSize x y pt 1e-6)
                psInRange
                          
    return .Pictures $ catMaybes [ spikesToScreen <$> Just treePic
                                 , spikesToScreen <$> treeSelectedPic
                                 ]
mkClusterlessScreenPic _ _ =
  return $ text "Error - mkClusterlessPic called on non-clusterless opt"


------------------------------------------------------------------------------
clusterlessPointPic
  :: XChan -> YChan -> Double -> (ClusterlessPoint, MostRecentTime) -> Picture
clusterlessPointPic (XChan x) (YChan y) tNow (cP,t) =
  translate (r2 $ pointD cP (Depth x)) (r2 $ pointD cP (Depth y)) $
  Pictures [ color (pointColor tNow t) $
             circleSolid (1e-6 + log (realToFrac $ pointW cP) / spikeScale)
           , circle (1e-6 + log (r2 $ pointW cP) / spikeScale)
           ]


------------------------------------------------------------------------------
trackToScreen :: Picture -> Picture
trackToScreen = scale trackScale trackScale

spikesToScreen :: Picture -> Picture
spikesToScreen = translate (-150) (-300) . scale spikeScale spikeScale

screenToSpikes :: Float -> Float -> (Float,Float)
screenToSpikes x y = ( (x+150)/spikeScale, (y+300)/spikeScale )

optsToScreen :: Picture -> Picture
optsToScreen = translate 0 0 . scale 10 10
  --translate 150 (-300) . scale 10 10

decodeColormap :: Field -> Field
decodeColormap = V.map (max 0 . (/(1-v)) . (subtract v))
  where v = 0.25

------------------------------------------------------------------------------
fieldPic :: Field -> Picture
fieldPic = drawNormalizedField . V.zip (trackBins0 defTrack)

spikeScale, trackScale :: Float
spikeScale = 2000000
trackScale = 400



------------------------------------------------------------------------------
pointAtSize
  :: XChan -> YChan -> ClusterlessPoint -> Double -> Picture
pointAtSize (XChan x) (YChan y) cP s =
  translate (r2 $ pointD cP (Depth x)) (r2 $ pointD cP (Depth y)) $
  circleSolid (r2 s)


------------------------------------------------------------------------------
treePicture
  :: XChan -> YChan -> Double -> KDMap ClusterlessPoint MostRecentTime
     -> Picture
treePicture xC yC tNow =
  Pictures . map (clusterlessPointPic xC yC tNow) . toList


------------------------------------------------------------------------------
pointColor :: Double -> MostRecentTime -> Color.Color
pointColor tNow (MostRecentTime t) = Color.makeColor r g b 1
  where e tau = exp (-1 * (max dt 0) / tau)
        dt = (r2 tNow) - (r2 t)
        r  = e 0.2
        g  = e 1
        b  = e 20


------------------------------------------------------------------------------
makeHistogramScreenPic :: Histogram Double -> Float -> Float -> IO Picture
makeHistogramScreenPic h sizeX sizeY = do
  cnts <- atomically $ M.getElems (h^.counts)
  return $ Pictures [ scaledBars cnts, reportPic cnts ]
    where
      nBins       = length
      totalCnt    = sum  :: [Int] -> Int
      mean cnts   = (/fI (max 1 $ totalCnt cnts)) . sum $
                    zipWith (*)
                    (fromIntegral <$> cnts)
                    (realToFrac <$> (V.toList $ h^.binEdges))
      mode cnts   = let m      = maximum cnts
                        mPairs = filter ((==m) . fst)
                                 (zip cnts . map r2 . V.toList $ h^.binEdges)
                    in maybe 0 snd (headMay mPairs)
      report cnts = unwords ["Mean: ", showWithUnit (mean cnts)
                            ," Max:" , showWithUnit (mode cnts :: Float)
                            ," Cnt:" , showWithUnit (fI $ nBins cnts)
                            ]
      reportPic cnts = scale 0.1 0.1 . text $ report cnts
      cntToHeight cnts y = log (fI y) * sizeY / log (fI $ maximum cnts)
      barWidth    cnts   = sizeX / (fromIntegral (nBins cnts))
      barLeft     cnts i = barWidth cnts * fromIntegral i + barWidth cnts / 2
      oneBarPic   cnts i c = let barH = cntToHeight cnts c
                             in translate (barLeft cnts i) (barH/2) $
                                rectangleSolid (barWidth cnts) barH
      scaledBars cnts = Pictures $
                        zipWith (oneBarPic cnts) [0..nBins cnts - 1] cnts

------------------------------------------------------------------------------
fI :: Int -> Float
fI = fromIntegral

showWithUnit :: (RealFrac a, Show a) => a -> String
showWithUnit x
  | x > 1000 = (pad3 . show $ floor (x/1000)) ++ "kSec"
  | x > 1    = (pad3 . show $ floor x)        ++ "sec"
  | x > 1e-3 = (pad3 . show $ floor (x*1000)) ++ "ms"
  | x > 1e-6 = (pad3 . show $ floor (x*1e6))  ++ "us"
  | x > 1e-9 = (pad3 . show $ floor (x*1e9))  ++ "ns"
  | otherwise = show x
  where pad3 s = L.replicate (length s - 3) ' ' ++ s
