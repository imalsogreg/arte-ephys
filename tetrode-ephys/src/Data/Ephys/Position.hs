{-|
Module      : Data.Ephys.Position
Description : Rat positions in room coordinates
Copyright   : (c) 2015 Greg Hale, Shea Levy
License     : BSD3
Maintainer  : imalsogreg@gmail.com
Stability   : experimental
Portability : GHC, Linux
-}
{-# LANGUAGE TemplateHaskell, BangPatterns, DeriveGeneric #-}

module Data.Ephys.Position where


import Data.Ephys.EphysDefs

import Data.Graph
import Control.Lens
import Pipes
import Data.Serialize
import GHC.Generics
import Data.Complex

-- | Position of the head in room coordinates, in meters
data Location = Location { _x :: !Double -- ^ X axis
                         , _y :: !Double -- ^ Y axis
                         , _z :: !Double -- ^ Z axis
                         }
              deriving (Eq, Ord, Show, Generic)

-- | Rotations to bring the room's coordinate frame in line with the animal's
--   coordinate frame, in radians
data Angle = Angle { _yaw :: !Double -- ^ Rotation of the X-Y plane about Z
                   , _pitch :: !Double -- ^ Rotation of the X-Z plane about Y
                   , _roll :: !Double -- ^ Rotation of the Y-Z plane about X
                   }
           deriving (Eq, Ord, Show, Generic)

-- | Measure of confidence in the position
data PosConf = ConfNone -- ^ No confidence
             | ConfUnsure -- ^ Unsure
             | ConfSure -- ^ Sure
             deriving (Eq, Ord, Show, Generic)

-- | Full 3D position data in room coordinates.
data Position = Position { -- | The time the position was taken
                           _posTime        :: !ExperimentTime
                           -- | Position of head in room coordinates, in meters
                         , _location       :: !Location
                           -- | Angle of the head with respect to the room,
                           --   in radians
                         , _angle          :: !Angle
                           -- | Direction of the head relative to the room's
                           --   positive X axis, in radians
                         , _heading        :: !Double
                           -- | Speed of the rat in meters per second
                         , _speed          :: !Double
                           -- | Confidence in the position
                         , _posConfidence  :: !PosConf
                           -- | Previous headings, head is most recent
                         , _headingHistory :: [Double]
                           -- | Previous speeds, head is most recent
                         , _speedHistory   :: [Double]

                           -- | Time of last confident position
                         , _lastGoodTime   :: !ExperimentTime
                           -- | Location of last confident position
                         , _lastGoodLoc    :: !Location
                         }
              deriving (Eq, Ord, Show, Generic)

------------------------------------------------------------------------------
-- | Center of the room along the X axis, with no history
nullPosition :: Position
nullPosition = Position (-1e6) (Location 0 0 0) (Angle 0 0 0) 0 0
               ConfNone [] [] (-1e6) (Location 0 0 0)

instance Serialize Location
instance Serialize Angle
instance Serialize PosConf
instance Serialize Position

$(makeLenses ''Location)
$(makeLenses ''Angle)
$(makeLenses ''Position)

-- | Update a position a single step
stepPos :: Position -- ^ The original position
        -> ExperimentTime -- ^ The time of the update
        -> Location -- ^ The new location
        -> Angle -- ^ The new angle
        -> PosConf -- ^ The new confidence
        -> Position -- ^ The new position
stepPos p0 t loc ang conf =
  Position t loc' ang heading' speed' conf' hHist' sHist' lastGoodTime' lastGoodLoc'
    where dt = t - p0^.posTime
          supressThis   = (conf == ConfNone) || (locDist (p0^.lastGoodLoc) loc > maxFrameDiff)
          takeEvenIfFar = (conf > ConfNone)  && (t - (p0^.lastGoodTime) > posUnstickTime)
--          takeEvenIfFar = False
          loc'           = if (not supressThis) || takeEvenIfFar
                           then loc
                           else (p0^.lastGoodLoc)
          (lastGoodTime',lastGoodLoc') =
            if (not supressThis) || takeEvenIfFar
            then (t,loc')
            else ((p0^.lastGoodTime),(p0^.lastGoodLoc))
          conf'          = min conf (if supressThis && (not takeEvenIfFar) then ConfNone else ConfSure)

          (dx,dy,_)      = locDiff (p0^.location) loc'
          instantHeading = atan2 dy dx  --TODO: Yep.
          hHist'         = instantHeading : init (p0^.headingHistory)
          heading'       = circMean hHist'
          instantSpeed   = locDist (p0^.location) loc' / dt *
                           (circLengthNorm hHist')
          sHist'         = (min 100 instantSpeed) : init (p0^.speedHistory)
          speed'         = mean sHist'
          posUnstickTime = 1
          maxFrameDiff   = 0.1

-- | Running position producer
producePos :: (Monad m) => Pipe Position Position m r
producePos = await >>= go
    where go p0 = do
            p <- await
            let p' = stepPos p0 (p^.posTime) (p^.location)
                     (p^.angle) (p^.posConfidence)
            yield p'
            go p'

-- | Vector difference between two locations
locDiff :: Location -- ^ The minuend
        -> Location -- ^ The subtrahend
        -> (Double,Double,Double)
locDiff a b = (dx, dy, dz)
  where dx = b^.x - a^.x
        dy = b^.y - a^.y
        dz = b^.z - a^.z

-- | Squared euclidian distance between two locations
locSqDist :: Location -> Location -> Double
locSqDist a b = (b^.z - a^.z)^(2::Int)
                + (b^.y - a^.y)^(2::Int)
                + (b^.x - a^.x)^(2::Int)

-- | Euclidian distance between two locations
locDist :: Location -> Location -> Double
locDist a b = sqrt $ locSqDist a b

-- | Arithmetic mean of a list
mean :: [Double] -> Double
mean xs = sum xs / (fromIntegral . length $ xs)

-- | Mean resultant vector of a list of angles, in radians
mrv :: [Double] -> Complex Double
mrv angs = let mrvX = sum . map cos $ angs
               mrvY = sum . map sin $ angs
           in  mrvX :+ mrvY

-- | Angle of the mean resultant vector of a list of angles
circMean :: [Double] -> Double
circMean angs =
  let (r :+ i) = mrv angs
  in  atan2 i r  -- TODO - is this right?

-- | Magnitude of the mean resultant vector, normalized to the number of angles
--
--   Measure of coherence: 0 means the vectors are spread out, 1 all aligned
circLengthNorm :: [Double] -> Double
circLengthNorm angs =
  let (r :+ i) = mrv angs in
  (r^(2::Int) + i^(2::Int)) /
  ((^(2::Int)) . fromIntegral $ length angs)
  
