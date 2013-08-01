module LfpViewerState where

import Control.Concurrent.STM
import LfpTraceData
import TraceGeometry
import RenderLfp
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW
import Control.Monad
import Data.ScrollBuffer
import Data.Time

import Control.Concurrent

data SubTrace = SubTrace { traceData   :: (TVar TraceData)
                         , boundingBox :: BoundingBox
                         , traceOpts   :: TraceOpts
                         }

data ViewerState = ViewerState { renderer   :: [SubTrace] -> IO ()
                              , subTraces  :: [SubTrace]
                              }
                  


defaultUpdate :: ViewerState -> IO ()
defaultUpdate st = forever $ do
  threadDelay $ 1000
  (UTCTime _ tDiff) <- getCurrentTime
  atomically $ do
    forM_ (subTraces st) 
      (\t -> modifyTVar (traceData t)
       (\sb -> advanceOne sb (0.1 * sin (realToFrac tDiff * 2 * 3.14))) )
      


defaultRenderer :: [SubTrace] -> IO ()
defaultRenderer subTs = do
  -- GL.clearColor  $= GL.Color4 0.1 0.1 0.1 1
  GL.blend       $= GL.Enabled
  GL.blendFunc   $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth   $= 2.0
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  (_, GL.Size xres yres) <- GL.get GL.viewport
  GL.ortho2D 0 0 (fromIntegral xres) (fromIntegral yres)
  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity
  mapM_ renderSubTrace subTs
  
renderSubTrace :: SubTrace -> IO ()
renderSubTrace subT = do
  renderBoundingBox (boundingBoxGeometry (boundingBox subT))
  td <- atomically $ readTVar (traceData subT)
  renderLfp      (traceGeometry td  (traceOpts subT) (boundingBox subT))

defaultTestState :: IO ViewerState
defaultTestState = atomically $ do
  let (td, to, bb) = newTrace 1000 2 (-0.5,-0.8) (1,0.8) 0.1 10 blue 0.2
      (td2,to2,bb2)= newTrace 1000 2 (-0.5, 0.1) (1,0.8) 0.1 10 blue 0.2
  td_t <- newTVar td
  td_t2<- newTVar td2
  let theSubtrace = SubTrace { traceData = td_t, boundingBox = bb, traceOpts = to }
      theSubtrace2= SubTrace { traceData = td_t2, boundingBox = bb2, traceOpts = to2 }
      theSubTraces = [theSubtrace, theSubtrace2]
  return $ ViewerState { LfpViewerState.renderer = defaultRenderer
                       , subTraces = theSubTraces
                       }
    

  