module RenderLfp where

import Control.Monad (zipWithM_)
import qualified Graphics.Rendering.OpenGL.GL as GL
import TraceGeometry


type GVertex   = (GL.Vertex3 GL.GLfloat)

purple :: GColor
purple = GL.Color4 1 1 0 1

{-
-- Not used - using renderPLine instead?
renderLine :: (GVertex, GVertex) -> (GColor, GColor) -> IO ()
renderLine (p1,p2) (c1,c2) = do
  GL.color  c1
  GL.vertex p1
  GL.vertex p2
  -}
           

renderPLine :: [GVertex] -> [GColor] -> IO ()
renderPLine vs cs = if ok then zipWithM_ (\v c -> do; GL.color c; GL.vertex v) vs cs
                    else return ()
  where ok = (length vs > 1)

               
renderLfp :: LfpTraceG -> IO ()
renderLfp lfpG = do
  let gname           = traceName  lfpG
      pts             = trace lfpG
      cursInd         = oldHeadInd lfpG
      ptColors        = colors lfpG
      olds            = drop cursInd
      news            = take cursInd
  GL.preservingMatrix $ GL.renderPrimitive GL.LineStrip $ renderPLine (news pts) (news ptColors)
  GL.preservingMatrix $ GL.renderPrimitive GL.LineStrip $ renderPLine (olds pts) (olds ptColors)
{-  GL.preservingMatrix $ GL.renderPrimitive GL.Lines $ do
    renderPLine (olds pts) (olds ptColors)
  GL.preservingMatrix $ GL.renderPrimitive GL.Lines $ do
    renderPLine (news pts) (news ptColors) -}
  
renderBoundingBox ::BoundingBoxG -> IO ()
renderBoundingBox bb = GL.preservingMatrix $ GL.renderPrimitive GL.Polygon $ do
                       renderPLine (outerFrame bb) (outerColors bb)

  
  {-
renderAll :: LfpTraceG -> TraceOpts -> BoundingBox -> IO ()
renderAll lfpG opt bb = do
  renderBoundingBox bb opt
  renderLfp lfpG
-}