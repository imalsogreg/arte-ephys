module TraceGeometry where

import Data.Fixed (mod')
import GHC.Float (double2Float)
import Data.Vector (Vector, take, drop, copy, length, toList)
import LfpTrace
import Graphics.Rendering.OpenGL.GL as GL
import Prelude hiding (take, drop, length)


data LfpTraceG = LfpTraceG { name      :: GL.Name
                           , trace     :: [GL.Vertex3 GL.GLfloat]
                           , cursorInd :: Int
                           , colors    :: [GL.Color4  GL.GLfloat]
                           } deriving (Show)
                   

traceGeometry :: TraceData -> TraceOpts -> BoundingBox -> LfpTraceG
traceGeometry td opt bb = LfpTraceG {name      = tracesName 
                                    ,trace     = dataToGLPoints td bb
                                    ,colors    = dataToColors td opt
                                    ,cursorInd = timeToInd td opt}
                                    

timeToInd :: TraceData -> TraceOpts -> Int
timeToInd td opt = floor (timeMod * dt)
  where timeMod = timeCursor td `mod'` timeBase opt
        dt = (timeBase opt) / (sampleFreq opt)
 
indToPx :: Int -> Int -> BoundingBox -> GL.GLfloat
indToPx i maxI bb = fromIntegral (i `div` maxI) * (sx-m) + realToFrac x0 + m/2
  where (GL.Vertex3 x0 _ _)   = boxOrigin bb
        (GL.Size c_sx _)      = boxSize   bb
        sx                    = fromIntegral c_sx
        m                     = boxMargin bb
                
                           
uVoltToPx :: Double -> BoundingBox -> GL.GLfloat
uVoltToPx v bb
  | y > sy/2       = sy/2
  | y < (-sy/2)    = (-sy/2)
  | otherwise      = y
  where (GL.Size _ c_sy) = boxSize bb
        sy               = fromIntegral c_sy
        y                = realToFrac v * sy
        
dataToColors :: TraceData -> TraceOpts -> [GL.Color4 GL.GLfloat]
dataToColors _ _ = repeat blue                     
        
dataToGLPoints :: TraceData -> BoundingBox -> [GL.Vertex3 GL.GLfloat]
dataToGLPoints td bb = zipWith (\v ind -> GL.Vertex3 (x ind) (y v) 0) voltUVs [0..]
  where voltUVs = toList (dataVec td)
        maxI    = length (dataVec td) - 1
        x i     = indToPx i maxI bb
        y v     = uVoltToPx v bb


black :: GL.Color4 GL.GLfloat
black = GL.Color4 0 0 0 1

red :: GL.Color4 GL.GLfloat
red = GL.Color4 1 0 0 1

green :: GL.Color4 GL.GLfloat
green = GL.Color4 0 1 0 1

blue :: GL.Color4 GL.GLfloat
blue = GL.Color4 0 0 1 1

tracesName :: GL.Name
tracesName = GL.Name 1
