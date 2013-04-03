module RenderLfp where

import Control.Monad (zipWithM_)
import qualified Graphics.Rendering.OpenGL.GL as GL
import TraceGeometry


type GVertex   = (GL.Vertex3 GL.GLfloat)

purple :: GColor
purple = GL.Color4 1 1 0 1

renderLine :: (GVertex, GVertex) -> (GColor, GColor) -> IO ()
renderLine (p1,p2) (c1,c2) = do
  GL.color  c1
  GL.vertex p1
  GL.vertex p2
             
renderPLine :: [GVertex] -> [GColor] -> IO ()
renderPLine vs cs = GL.renderPrimitive GL.Points $ 
                    zipWithM_ (\v c -> do; GL.color c; GL.vertex v) vs cs
    
renderLfp :: LfpTraceG -> IO ()
renderLfp lfpG = do
  let gname         = traceName  lfpG
      pts           = trace lfpG
      cursInd       = cursorInd lfpG
      ptColors      = colors lfpG
      olds          = drop cursInd
      news          = take cursInd
  renderPLine (olds pts) (olds ptColors)
  renderPLine (news pts) (news ptColors)
  
renderBoundingBox ::BoundingBoxG -> IO ()
renderBoundingBox bb = renderPLine (outerFrame bb) (outerColors bb)

  
  {-
renderAll :: LfpTraceG -> TraceOpts -> BoundingBox -> IO ()
renderAll lfpG opt bb = do
  renderBoundingBox bb opt
  renderLfp lfpG
-}