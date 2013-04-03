module LfpTrace where

import qualified Graphics.Rendering.OpenGL as GL
import Data.Vector

data TraceData = TraceData { timeCursor :: Double
                           , dataVec    :: (Vector Double)
                           } deriving (Show)

data BoundingBox = BoundingBox { boxOrigin :: GL.Vertex3 GL.GLfloat
                               , boxSize   :: GL.Size
                               , pxPerUV   :: Double
                               , boxMargin :: GL.GLfloat
                               }
                   deriving (Show)
                            
data TraceOpts = TraceOpts { traceColor :: (GL.Color4 GL.GLfloat)
                           , traceWidth :: GL.GLfloat
                           , timeBase   :: Double
                           , sampleFreq :: Double
                           } deriving (Show)
                            
draw :: BoundingBox -> TraceOpts -> TraceData -> IO ()
draw box opts signal = undefined
  