module RenderLfp where

import Control.Monad (zipWithM_)
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL.VertexSpec
import TraceGeometry

type GColor = (GL.Color4 GL.GLfloat)
type GVertex   = (GL.Vertex3 GL.GLfloat)

purple :: GColor
purple = GL.Color4 1 1 0 1

renderSeg :: (GVertex, GVertex) -> (GColor, GColor) -> IO ()
renderSeg (p1,p2) (c1,c2) = do
  GL.color  purple
  GL.vertex p1
  GL.vertex p2
             
renderLfp :: LfpTraceG -> IO ()
renderLfp lfpG = do
  let gname      = name  lfpG
      pts        = trace lfpG
      cursInd    = cursorInd lfpG
      ptColors   = colors lfpG
      ptPairs    = if (not $ null pts) 
                   then zipWith (,) (init pts) (tail pts)
                   else []
      colorPairs = if (not $ null ptColors)
                   then zipWith (,) (init ptColors) (tail ptColors)
                   else repeat (blue, blue)
  zipWithM_ (\pPair cPair -> renderSeg pPair cPair) ptPairs colorPairs
  
  
