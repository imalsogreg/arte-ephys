module LfpTraceData where

import qualified Graphics.Rendering.OpenGL as GL
import RenderLfp
import Data.Vector

data TraceData = TraceData { timeCursor :: Double
                           , dataVec    :: (Vector Double)
                           } deriving (Show)

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
traceNSamp sampHz windowLenSec = floor . (*)
                                      
newTraceData :: Double -> Double -> TraceData
netTraceData sampHz windowLenSec = TraceData { timeCursor = 0
                                             , dataVec = fromList data0 }
  where data0 = replicate (traceNSamp sampHz windowLenSec) 0
  
newBoundingBox :: (
        
newTrace :: Double -> Double -> (TraceData, TraceOpts, BoundingBox)
newTrace sampHz windowLenSec = 
  
  (td, opt, bb)


-- This is another module's job.
draw :: BoundingBox -> TraceOpts -> TraceData -> IO ()
draw box opts signal = undefined
  