module LfpTraceData where

import qualified Graphics.Rendering.OpenGL as GL
import Data.ScrollBuffer
--import Data.Vector hiding (replicate)

{-
data TraceData = TraceData { timeCursor :: Double
                           , dataVec    :: (Vector Double)
                           } deriving (Show)
-}

type TraceData = ScrollBuffer Double

data BoundingBox = BoundingBox { boxOrigin :: GL.Vertex3 GL.GLfloat
                               , boxSize   :: GL.Vertex2 GL.GLfloat
                               , pxPerUV   :: Double
                               , boxMargin :: GL.GLfloat
                               }
                   deriving (Show)
                            
data TraceOpts = TraceOpts { traceColor :: (GL.Color4 GL.GLfloat)
                           , traceWidth :: GL.GLfloat
                           , timeBase   :: Double
                           , sampleFreq :: Double
                           } deriving (Show)
                            
                                      
                                      
traceNSamp :: Double -> Double -> Int
traceNSamp sampHz windowLenSec = floor (sampHz * windowLenSec)
                                      
-- This one is useful.  Do I really need smart constructors for
-- bounding box and traceopts?
newTraceData :: Double -> Double -> TraceData
newTraceData sampHz windowLenSec = fromList data0 
  where data0 = replicate (traceNSamp sampHz windowLenSec) 0
  

newTraceOpts :: Double -> Double -> (GL.Color4 GL.GLfloat) -> GL.GLfloat -> TraceOpts
newTraceOpts sampHz windowLenSec traceC lineW =
  TraceOpts { traceColor=traceC, traceWidth=lineW, timeBase=windowLenSec, sampleFreq=sampHz }
              
  
newTrace :: Double ->                             -- samplerate
            Double ->                             -- window length
            (Double,Double) ->                    -- Lower left (x,y)
            (Double,Double) ->                    -- width, height
            Double ->                             -- Bounding box margin
            Double ->                             -- px per microvolt
            (GL.Color4 GL.GLfloat) ->             -- Trace color
            Double ->                             -- Line width
            (TraceData, TraceOpts, BoundingBox)
newTrace sampHz windowLenSec (x,y) (sx,sy) marg dispGain traceC lineW = 
  let td = newTraceData sampHz windowLenSec 
      bb = BoundingBox {boxOrigin  = (GL.Vertex3 (realToFrac x) (realToFrac y) 0.1)
                       , boxSize   = (GL.Vertex2 (realToFrac sx) (realToFrac sy))
                       , pxPerUV   = dispGain
                       , boxMargin = realToFrac marg}
      opt = newTraceOpts sampHz windowLenSec traceC (realToFrac lineW) in
  (td, opt, bb)

--testTrace :: (TraceData, TraceOpts, BoundingBox)
--testTrace = newTrace 2000 2 (0.1, 0.1) (0.9,0.9) 0.05 10 green 1

green :: GL.Color4 GL.GLfloat
green = GL.Color4 0 1 0 1
  
