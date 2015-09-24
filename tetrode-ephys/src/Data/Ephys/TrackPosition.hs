{-|
Module      : Data.Ephys.TrackPosition
Description : Rat positions in decoding reference frame
Copyright   : (c) 2015 Greg Hale, Shea Levy
License     : BSD3
Maintainer  : imalsogreg@gmail.com
Stability   : experimental
Portability : GHC, Linux
-}
{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module Data.Ephys.TrackPosition where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Applicative ((<$>),(<*>))
import           Data.Graph
import           Data.List           (sortBy)
import qualified Data.Map            as Map
import           Data.Monoid
import           Data.Ord            (comparing)
import           Data.SafeCopy
import qualified Data.Vector         as V
------------------------------------------------------------------------------
import Data.Ephys.Position


------------------------------------------------------------------------------
-- | A physical track segment
--
--  <<TrackBin.png>>
data TrackBin =
  TrackBin { -- | Identifier for the bin to define trajectories
             _binName :: !String
             -- | Origin of the track bin in room coordinates
           , _binLoc  :: !Location
             -- | The outgoing direction, in radians
           , _binDir  :: !Double -- radians
             -- | Distance from the origin to the beginning of the bin
             --   in meters
           , _binA    :: !Double
             -- | Distance from the origin to the end of the bin in meters
           , _binZ    :: !Double
             -- | The width (i.e. the dimension perpendicular to the outgoing
             --   direction) of the bin, in meters.
           , _binWid  :: !Double
             -- | The shape of the bin
           , _binCaps :: !BinCaps
           } deriving (Eq, Ord, Show)

-- | A bin shape
data BinCaps = CapCircle -- ^ A circular bin for choice points on the track
             | CapFlat (Double,Double)
             -- ^ A rectangular bin for linear track segments. Parameter is
               --   a tuple of the angle between the bin and its predecessor and
               --   the angle between the bin and its successor.
               --  <<CapEnd.png>>
             deriving (Eq, Ord, Show)

$(makeLenses ''TrackBin)

-- | A representation of the track
data Track = Track { _trackBins  :: [TrackBin] -- ^ The bins of the track
                   } deriving (Eq, Show)

-- | A position on the track suited for decoding analysis
data TrackPos = TrackPos { -- | The bin the position is in
                           _trackBin :: !TrackBin
                           -- | The direction the rat is going in relative to
                           --   the bin
                         , _trackDir :: !TrackDirection
                           -- | Whether the rat is in bounds or not
                         , _trackEcc :: !TrackEccentricity
                         } deriving (Eq, Ord, Show)

-- | A direction on the track relative to a given bin's direction
data TrackDirection = Outbound -- ^ In line with the bin's direction
                    | Inbound -- ^ Against the bin's direction
                    deriving (Eq, Ord, Show)

-- | Inside or outside of the track
data TrackEccentricity = OutOfBounds  -- ^ Out of the track's bounds
                       | InBounds -- ^ In the track's bounds
                       deriving (Eq, Ord, Show)

$(makeLenses ''Track)
$(makeLenses ''TrackPos)

-- | Generate track positions from a track
allTrackPos :: Track -- ^ The track of interest
            -> V.Vector TrackPos
allTrackPos t =
  V.fromList [TrackPos bin dir ecc | bin <- t^.trackBins
                                   , dir <- [Inbound,Outbound]
                                   , ecc <- [OutOfBounds,InBounds]]

-- Use mapping from track bin to a to model 'fields' in general
-- ie an instantaneous occupancy field, a trial-sum occupancy
-- field, or a spike rate field
-- | A real-valued quantity associated with a track by position
type Field = V.Vector Double

-- | A 'Field' where each value is paired with its associated position
type LabeledField a = V.Vector (TrackPos, a)

-- | Label a 'Field' with respect to a given 'Track'
labelField :: Track -- ^ The track to label with respect to
           -> Field -- ^ The field to label
           -> LabeledField Double
labelField t f = V.zip (allTrackPos t) f

-- ^ Smoothing kernel for camera position
data PosKernel = PosDelta -- ^ Delta function
                 -- | Gaussian smoothing. Parameter is standard deviation
               | PosGaussian Double

------------------------------------------------------------------------------
-- | Turn a position into an instantaneous field
posToField :: Track -- ^ The track the position is on
           -> Position -- ^ The position we're transforming
           -> PosKernel -- ^ The smoothing kernel to use
           -> Field
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
-- | Translate from room coordinates to track bin coordinates
relativeCoords :: TrackBin -- ^  The bin for the final reference frame
               -> (Double,Double) -- ^ The location in room coordinates
               -> (Double,Double)
relativeCoords bin (x',y') =
  let th = (-1 * bin^.binDir)
      dx = x' - bin^.binLoc.x
      dy = y' - bin^.binLoc.y
  in
   (dx * cos th - dy * sin th, dx * sin th + dy * cos th)

-- | Find the closest bin to a given position.
trackClosestBin :: Track -- ^ The track the position is on
                -> Position -- ^ The position to find a bin near
                -> TrackBin
trackClosestBin track pos =
  head . sortBy (comparing (posBinDistSq pos)) $
  (track ^. trackBins)

-- | Squared distance between a position and the center of a bin, in m^2
posBinDistSq :: Position -- ^ The position
             -> TrackBin -- ^ The bin
             -> Double
posBinDistSq pos bin = locSqDist (bin^.binLoc)
                       (pos^.location)

-- | Create a circular track
circularTrack :: (Double,Double) -- ^ Center of the track in room coordinates in meters
                 -> Double       -- ^ radius in meters
                 -> Double       -- ^ height in meters
                 -> Double       -- ^ track width in meters
                 -> Double       -- ^ bin length in meters
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

-- | A maze with a central platform and arms radiating outward
radialArmMaze :: (Double, Double) -- ^ Center of track in room coords
              -> Double           -- ^ Start arm angle from x axis (radians)
              -> Double           -- ^ Home platform radius
              -> Int              -- ^ Number of arms
              -> Double           -- ^ Origin to arm tip distance (meters)
              -> Double           -- ^ Height (meters)
              -> Double           -- ^ Track width (meters)
              -> Double           -- ^ Bin length (meters)
              -> Track
radialArmMaze (x0,y0) a0 rPlat nArm lenArm h w binLen = Track $ plat : arms
 where plat  = TrackBin "c" (Location x0 y0 h) a0 (negate rPlat) rPlat
               rPlat CapCircle
       fI    = fromIntegral
       arms  = concatMap arm [0..nArm-1]
       arm n = let ang     = 2 * pi * fI n / fI nArm + a0
                   nSeg    = floor ((lenArm - rPlat) / binLen)
                   binLen' = (lenArm - rPlat) / fI nSeg
                   seg m   =
                     let r = (fI m+0.5)*binLen' + rPlat
                     in if m < nSeg - 1
                        then
                         TrackBin (show n <> "." <> show n)
                         (Location (r * cos ang) (r * sin ang) h)
                         ang (-0.5 * binLen') (0.5 * binLen') w
                         (CapFlat (0,0))
                        else
                         TrackBin (show n <> ".e")
                         (Location (r * cos ang) (r * sin ang) h)
                         ang (-0.5 * binLen') (0.5 * binLen') w
                         CapCircle
                in map seg [0 .. nSeg-1]

------------------------------------------------------------------------------
-- | Zip a function over two fields
updateField :: (Double->Double->Double) -- ^ The function to combine each pair of values
            -> Field -- ^ The first field
            -> Field -- ^ The second field
            -> Field
updateField = V.zipWith
{-# INLINE updateField #-}

{-  -- TODO - serialize TrackPos
putTrackPos :: Put TrackPos
putTrackPos tp = do
  put $ tp^.

-}
