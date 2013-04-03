module TraceGeometry where

import Data.Fixed (mod')
import GHC.Float (double2Float)
import Data.Vector (Vector, take, drop, copy, length, toList)
import LfpTraceData
import Graphics.Rendering.OpenGL.GL as GL
import Prelude hiding (take, drop, length)

type GPoint = GL.Vertex3 GL.GLfloat
type GColor = GL.Color4  GL.GLfloat

data LfpTraceG = LfpTraceG { traceName :: GL.Name
                           , trace     :: [GPoint]
                           , cursorInd :: Int
                           , colors    :: [GColor]
                           } deriving (Show)
                   
data BoundingBoxG = BoundingBoxG { boxName     :: GL.Name
                                 , outerFrame  :: [GPoint]
                                 , outerColors :: [GColor]
                                 , innerFrame  :: [GPoint]
                                 , innerColors :: [GColor]
                                 , ticPoints   :: [(GPoint, GPoint)]
                                 , gridPoints  :: [(GPoint, GPoint)]
                                 , auxColor    :: GColor
                                 }

traceGeometry :: TraceData -> TraceOpts -> BoundingBox -> LfpTraceG
traceGeometry td opt bb = LfpTraceG {traceName = tracesName 
                                    ,trace     = dataToGLPoints td bb
                                    ,colors    = dataToColors td opt
                                    ,cursorInd = timeToInd td opt}

                          
boundingBoxGeometry :: BoundingBox -> TraceOpts -> BoundingBoxG
boundingBoxGeometry bb opt =
  BoundingBoxG { boxName = boxesName
               , outerFrame = rectanglePoints (boxOrigin bb) (boxSize bb)
               , outerColors = repeat green
               , innerFrame = rectanglePoints innerOrigin innerSize
               , innerColors = repeat green
               , ticPoints = []
               , gridPoints = []
               , auxColor = black } where 
    margin = boxMargin bb
    innerOrigin = case (boxOrigin bb) of
      (GL.Vertex3 x y z) -> GL.Vertex3 (x + margin) (y + margin) z
    innerSize = case (boxSize bb) of
      (GL.Vertex2 sx sy) -> GL.Vertex2 (sx - 2 * margin) (sy - 2 * margin)

                              
                              
timeToInd :: TraceData -> TraceOpts -> Int
timeToInd td opt = floor (timeMod * dt)
  where timeMod = timeCursor td `mod'` timeBase opt
        dt = (timeBase opt) / (sampleFreq opt)
 
indToPx :: Int -> Int -> BoundingBox -> GL.GLfloat
indToPx i maxI bb = fromIntegral (i `div` maxI) * (sx-m) + realToFrac x0 + m/2
  where (GL.Vertex3 x0 _ _)   = boxOrigin bb
        (GL.Vertex2 sx _)     = boxSize   bb
        m                     = boxMargin bb
                
                           
uVoltToPx :: Double -> BoundingBox -> GL.GLfloat
uVoltToPx v bb
  | y > sy/2       = (sy / 2)
  | y < (-sy/2)    = (-sy/2)
  | otherwise      = realToFrac y
  where (GL.Vertex2 _ sy) = boxSize bb
        y                   = realToFrac v * sy
        
dataToColors :: TraceData -> TraceOpts -> [GColor]
dataToColors _ _ = repeat blue                     
        
dataToGLPoints :: TraceData -> BoundingBox -> [GPoint]
dataToGLPoints td bb = zipWith (\v ind -> GL.Vertex3 (x ind) (y v) 0) voltUVs [0..]
  where voltUVs = toList (dataVec td)
        maxI    = length (dataVec td) - 1
        x i     = indToPx i maxI bb
        y v     = uVoltToPx v bb

                                    
rectanglePoints :: GPoint -> (GL.Vertex2 GL.GLfloat) -> [GPoint]
rectanglePoints (GL.Vertex3 x0 y0 z0) (GL.Vertex2 sx sy)  =
  [GL.Vertex3 x0 y0 z0
  , GL.Vertex3 (x0 + sx) y0 z0
  , GL.Vertex3 (x0 + sx) (y0 + sy) z0
  , GL.Vertex3 x0 (y0 + sy) z0
  , GL.Vertex3 x0 y0 z0
  ]
  
black :: GColor
black = GL.Color4 0 0 0 1

red :: GColor
red = GL.Color4 1 0 0 1

green :: GColor
green = GL.Color4 0 1 0 1

blue :: GColor
blue = GL.Color4 0 0 1 1

tracesName :: GL.Name
tracesName = GL.Name 1

boxesName :: GL.Name
boxesName = GL.Name 2