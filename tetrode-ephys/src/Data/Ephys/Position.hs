{-# LANGUAGE TemplateHaskell, BangPatterns, DeriveGeneric #-}

module Data.Ephys.Position where


import Data.Ephys.EphysDefs

import Data.Graph
import Control.Lens
import Pipes
import Data.Serialize
import GHC.Generics
--import qualified Data.Trees.KdTree as KD
import Data.Complex

data Location = Location {_x :: !Double, _y :: !Double, _z :: !Double}
              deriving (Eq, Ord, Show, Generic)

data Angle = Angle {_yaw :: !Double, _pitch :: !Double, _roll :: !Double}
           deriving (Eq, Ord, Show, Generic)

data PosConf = ConfNone | ConfUnsure | ConfSure
             deriving (Eq, Ord, Show, Generic)

-- Full 3D position data.  For position relative to a linear track, see TrackPos
data Position = Position { _posTime        :: !ExperimentTime
                         , _location       :: !Location
                         , _angle          :: !Angle
                         , _heading        :: !Double
                         , _speed          :: !Double
                         , _posConfidence  :: !PosConf
                         , _headingHistory :: [Double]
                         , _speedHistory   :: [Double]

                         , _lastGoodTime   :: !ExperimentTime
                         , _lastGoodLoc    :: !Location
                         }
              deriving (Eq, Ord, Show, Generic)

------------------------------------------------------------------------------
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

stepPos :: Position
        -> ExperimentTime
        -> Location
        -> Angle
        -> PosConf
        -> Position
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
          sHist'         = (clipMax 100 instantSpeed) : init (p0^.speedHistory)
          speed'         = mean sHist'
          posUnstickTime = 1
          maxFrameDiff   = 0.1

producePos :: (Monad m) => Pipe Position Position m r
producePos = await >>= go
    where go p0 = do
            p <- await
            let p' = stepPos p0 (p^.posTime) (p^.location)
                     (p^.angle) (p^.posConfidence)
            yield p'
            go p'

clipMax :: Double -> Double -> Double
clipMax m a = if a > m then m else a

locDiff :: Location -> Location -> (Double,Double,Double)
locDiff a b = (dx, dy, dz)
  where dx = b^.x - a^.x
        dy = b^.y - a^.y
        dz = b^.z - a^.z

locSqDist :: Location -> Location -> Double
locSqDist a b = (b^.z - a^.z)^(2::Int)
                + (b^.y - a^.y)^(2::Int)
                + (b^.x - a^.x)^(2::Int)

locDist :: Location -> Location -> Double
locDist a b = sqrt $ locSqDist a b

{-
-- TODO - risky to depend on KdTree?  Not very active package
instance KD.Point Location where
  dimension   = const 3
  coord 0 a   = a ^. x
  coord 1 a   = a ^. y
  coord 2 a   = a ^. z
  coord n _   = error $
                "Impossible coord from Location: " ++ show n
  dist2 a1 a2 = (a2 ^. x - a1 ^. x)^(2::Int) +
                (a2 ^. y - a1 ^. y)^(2::Int) +
                (a2 ^. z - a1 ^. z)^(2::Int)
-}

mean :: [Double] -> Double
mean xs = sum xs / (fromIntegral . length $ xs)

mrv :: [Double] -> Complex Double
mrv angs = let mrvX = sum . map cos $ angs
               mrvY = sum . map sin $ angs
           in  mrvX :+ mrvY

circMean :: [Double] -> Double
circMean angs =
  let (r :+ i) = mrv angs
  in  atan2 i r  -- TODO - is this right?

circLengthNorm :: [Double] -> Double
circLengthNorm angs =
  let (r :+ i) = mrv angs in
  (r^(2::Int) + i^(2::Int)) /
  ((^(2::Int)) . fromIntegral $ length angs)
  
