module Data.Ephys.TrackSpec where

import Data.Graph

import qualified Data.Ephys.Position as P

data SpecPoint = SpecPoint { pName  :: String
                           , binLoc :: P.Location
                           } deriving (Show)

data SpecEdge = SpecEdge { pointA :: P.Location
                         , ctrlA  :: P.Location
                         , pointB :: P.Location
                         , ctrlB  :: P.Location
                         }
              deriving (Show)

lineInterp :: P.Location -> P.Location -> Double -> P.Location
lineInterp (P.Location x0 y0 z0) (P.Location x1 y1 z1) frac =
  let h a b = frac * (b-a) + a
  in P.Location (h x0 x1) (h y0 y1) (h z0 z1)

unlengthedSplineInterp :: SpecEdge -> Double -> P.Location
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

data TrackSpec = CircularTrack Double Double Double Double Double
               | SplineTrack [SpecPoint] [SpecEdge]
