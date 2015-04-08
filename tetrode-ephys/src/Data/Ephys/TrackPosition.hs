{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module Data.Ephys.TrackPosition where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Applicative ((<$>),(<*>))
import           Data.Graph
import           Data.List           (sortBy)
import qualified Data.Map            as Map
import           Data.Ord            (comparing)
import           Data.SafeCopy  
import qualified Data.Vector         as V
------------------------------------------------------------------------------
import Data.Ephys.Position


------------------------------------------------------------------------------
data TrackBin =
  TrackBin { _binName :: !String
           , _binLoc  :: !Location
           , _binDir  :: !Double -- radians
           , _binA    :: !Double
           , _binZ    :: !Double
           , _binWid  :: !Double
           , _binCaps :: !BinCaps
           } deriving (Eq, Ord, Show)

data BinCaps = CapCircle
             | CapFlat (Double,Double)
             deriving (Eq, Ord, Show)

$(makeLenses ''TrackBin)

data Track = Track { _trackBins  :: [TrackBin]
                   } deriving (Eq, Show)

data TrackDirection = Outbound | Inbound
                    deriving (Eq, Ord, Show)

data TrackEccentricity = OutOfBounds | InBounds 
                       deriving (Eq, Ord, Show)

data TrackPos = TrackPos { _trackBin :: !TrackBin
                         , _trackDir :: !TrackDirection
                         , _trackEcc :: !TrackEccentricity
                         } deriving (Eq, Ord, Show)

$(makeLenses ''Track)
$(makeLenses ''TrackPos)

allTrackPos :: Track -> V.Vector TrackPos
allTrackPos t =
  V.fromList [TrackPos bin dir ecc | bin <- t^.trackBins
                                   , dir <- [Inbound,Outbound]
                                   , ecc <- [OutOfBounds,InBounds]]

-- Use mapping from track bin to a to model 'fields' in general
-- ie an instantaneous occupancy field, a trial-sum occupancy
-- field, or a spike rate field
type Field = V.Vector Double

type LabeledField a = V.Vector (TrackPos, a)

labelField :: Track -> Field -> LabeledField Double
labelField t f = V.zip (allTrackPos t) f

data PosKernel = PosDelta
               | PosGaussian Double

------------------------------------------------------------------------------
-- Turn a position into an instantaneous field
posToField :: Track -> Position -> PosKernel -> Field
posToField t pos kern =
    let distSq bin = locSqDist (pos^.location) (bin^.binLoc)
        binC       = trackClosestBin t pos
        tDir
          | cos (pos^.heading - binC^.binDir) > 0 = Outbound
          | otherwise                             = Inbound
        ecc b
          | (abs y') > (b^.binWid / 2) = OutOfBounds 
          | otherwise                  = InBounds
          where (_,y') = relativeCoords b (pos^.location^.x, pos^.location^.y)
        trackPosValUnNormalized :: TrackPos -> Double
        trackPosValUnNormalized tp = case kern of
          PosDelta    -> if tp^.trackBin == binC
                            && tp^.trackDir == tDir
                            && tp^.trackEcc == ecc binC
                         then 1 else 0
          PosGaussian sd ->
            if (tp^.trackEcc) == ecc binC && (tp^.trackDir) == tDir
            then exp( (-1) / (2 * sd * sd) * distSq (tp^.trackBin)  )
            else 0
        totalVal = V.foldl (\a tp -> a +  trackPosValUnNormalized tp) 0
                   (allTrackPos t :: V.Vector TrackPos)
        trackPosVal :: TrackPos -> Double
        trackPosVal tp = if totalVal > 0
                         then trackPosValUnNormalized tp / totalVal
                         else 1/ (fromIntegral $ V.length (allTrackPos t))
     in (V.map trackPosVal (allTrackPos t))

------------------------------------------------------------------------------
relativeCoords :: TrackBin -> (Double,Double) -> (Double,Double)
relativeCoords bin (x',y') =
  let th = (-1 * bin^.binDir)
      dx = x' - bin^.binLoc.x
      dy = y' - bin^.binLoc.y
  in
   (dx * cos th - dy * sin th, dx * sin th + dy * cos th)

trackClosestBin :: Track -> Position -> TrackBin
trackClosestBin track pos =
  head . sortBy (comparing (posBinDistSq pos)) $
  (track ^. trackBins)

posBinDistSq :: Position -> TrackBin -> Double
posBinDistSq pos bin = locSqDist (bin^.binLoc)
                       (pos^.location)

circularTrack :: (Double,Double) -- (x,y) in meters
                 -> Double       -- radius in meters
                 -> Double       -- height in meters
                 -> Double       -- track width in meters
                 -> Double       -- bin length in meters
                 -> Track
circularTrack (cX,cY) r h w tau =
  Track [aPoint t [n] | (t,n) <- zip thetaCs names]
  where
    fI = fromIntegral
    circumference = 2*pi*r
    nPts = floor (circumference / tau) :: Int
    tau' = circumference / fromIntegral nPts
    names = map (toEnum . (+  fromEnum 'A')) [0..nPts-1]
    thetaIncr = 2*pi/ fI nPts
    thetaCs = [0, thetaIncr .. 2*pi-thetaIncr]
    aPoint :: Double -> String -> TrackBin
    aPoint theta n =
      TrackBin n
      (Location (r * cos theta + cX) (r * sin theta + cY) h)
      (theta + pi/2)
      (-1 * tau' / 2) (tau' / 2)
      w
      (CapFlat (thetaIncr/(-2), thetaIncr/2))
------------------------------------------------------------------------------
updateField :: (Double->Double->Double) -> Field -> Field -> Field
updateField = V.zipWith
{-# INLINE updateField #-}

{-  -- TODO - serialize TrackPos
putTrackPos :: Put TrackPos
putTrackPos tp = do
  put $ tp^.

-}
