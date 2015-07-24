{-|
Module      : Data.Ephys.TrackSpec
Description : Specification of a track
Copyright   : (c) 2015 Greg Hale, Shea Levy
License     : BSD3
Maintainer  : imalsogreg@gmail.com
Stability   : experimental
Portability : GHC, Linux
-}
module Data.Ephys.TrackSpec where

import Data.Graph

import qualified Data.Ephys.Position as P

-- | A named point in the track
data SpecPoint = SpecPoint { pName  :: String -- ^ The point name
                           , binLoc :: P.Location -- ^ The point location
                           } deriving (Show)

-- TODO: Rewrite start and end points to be point names
-- | A directed track segment defined as a Bézier curve
data SpecEdge = SpecEdge { pointA :: P.Location -- ^ The start point A
                         , ctrlA  :: P.Location -- ^ The control point of A
                         , pointB :: P.Location -- ^ The end point B
                         , ctrlB  :: P.Location -- ^ The control point of B
                         }
              deriving (Show)

-- | Linear interpolation between two locations
lineInterp :: P.Location -- ^ The starting location
           -> P.Location -- ^ The ending location
           -> Double -- ^ Fraction of the distance between the locations
           -> P.Location
lineInterp (P.Location x0 y0 z0) (P.Location x1 y1 z1) frac =
  let h a b = frac * (b-a) + a
  in P.Location (h x0 x1) (h y0 y1) (h z0 z1)

-- | Compute a point on a Bézier curve
--
--   The spline displacement isn't linear in the parameter
unlengthedSplineInterp :: SpecEdge -- ^ The specification of the curve
                       -> Double -- ^ The Bézier parameter
                       -> P.Location
unlengthedSplineInterp (SpecEdge pA cA pB cB) i = let
  p0 = pA
  p1 = cA
  p2 = cB
  p3 = pB
  p4 = lineInterp p0 p1 i
  p5 = lineInterp p1 p2 i
  p6 = lineInterp p2 p3 i
  p7 = lineInterp p4 p5 i
  p8 = lineInterp p5 p6 i
  p9 = lineInterp p7 p8 i
  in p9

-- | A specification of a track
data TrackSpec = -- | A circular track, parameterized by center X coordinate,
                 --   center Y coordinate, radius, height, and track width, in
                 --   meters
                 CircularTrack Double Double Double Double Double
                 -- | A track defined by a list of splines.
                 --   Currently misdefined
               | SplineTrack [SpecPoint] [SpecEdge]
