----------------------------------------------------------------------
-- |
-- Module      : LfpViewer
-- Copyright   : (c) Greg Hale
-- License     : GPL-3
-- 
-- Maintainer  : Greg Hale <imalsogreg@gmail.com>
-- Stability   : unstable 
-- Portability :
--
-- LfpViewer   : 
--
----------------------------------------------------------------------

module Main where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.GLU.Raw as GLU

import Control.Monad (liftM, unless, when)

main :: IO ()
main = do
  configureDisplay
  start
  stop
  
configureDisplay :: IO ()
configureDisplay = do
  _ <- GLFW.initialize

  _ <- GLFW.openWindow GLFW.defaultDisplayOptions
      { GLFW.displayOptions_numRedBits   = 8
      , GLFW.displayOptions_numGreenBits = 8
      , GLFW.displayOptions_numBlueBits  = 8
      , GLFW.displayOptions_numDepthBits = 1
      }      
  GLFW.setWindowSizeCallback windowSizeCallback
  GL.clearColor    GL.$= GL.Color4 0.05 0.05 0.05 1
  GL.depthFunc     GL.$= Just GL.Less
  GL.colorMaterial GL.$= Just (GL.FrontAndBack, GL.AmbientAndDiffuse)
  GL.shadeModel    GL.$= GL.Smooth
  GL.lighting              GL.$= GL.Enabled
  GL.lightModelAmbient     GL.$= GL.Color4 0.2 0.2 0.2 1
  GL.position (GL.Light 0) GL.$= GL.Vertex4 (-10) 10 (-10) 0
  GL.ambient  (GL.Light 0) GL.$= GL.Color4 0.4 0.4 0.4 1
  GL.diffuse  (GL.Light 0) GL.$= GL.Color4 0.8 0.8 0.8 1
  GL.light    (GL.Light 0) GL.$= GL.Enabled

windowSizeCallback :: Int -> Int -> IO ()
windowSizeCallback w h = do
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GLU.gluPerspective 45 (fromIntegral w / fromIntegral h) 0.1 100

start :: IO ()
start = do
  putStrLn "starting"
  loop 0 0
  where
    loop xa ya = do
        drawD xa ya
        GLFW.resetTime

        q0 <- GLFW.keyIsPressed GLFW.KeyEsc
        q1 <- GLFW.keyIsPressed (GLFW.CharKey 'Q')
        unless (q0 || q1) $ do
            (jlr, jud) <- getJoystickDirections
            (klr, kud) <- getCursorKeyDirections

            let xa' = (xa +        jud * maxAngle) - kud
            let ya' = (ya + negate jlr * maxAngle) - klr

            t <- liftM (numSecondsBetweenFrames -) GLFW.getTime
            when (t > 0) (GLFW.sleep t)

            loop xa' ya'
      where
        maxAngle :: Float
        maxAngle = 1

        numSecondsBetweenFrames :: Double
        numSecondsBetweenFrames = recip (fromIntegral framesPerSecond)

        framesPerSecond :: Int
        framesPerSecond = 200

stop :: IO ()
stop = do
  putStrLn "closing"
  GLFW.closeWindow
  GLFW.terminate

getJoystickDirections :: IO (Float, Float)
getJoystickDirections = do
    r <- take 2 `fmap` GLFW.getJoystickPosition GLFW.Joystick0 2
    return $
      case r of
        [x, y] -> (x, y)
        _      -> (0, 0)

getCursorKeyDirections :: IO (Float, Float)
getCursorKeyDirections = do
    l <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyLeft
    r <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyRight
    u <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyUp
    d <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyDown
    return (-l + r, -u + d)
  where
    toFloat b = if b then 1 else 0

drawD :: Float -> Float -> IO ()
drawD xa ya = do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.loadIdentity

    GL.preservingMatrix $ do
        GL.rotate (realToFrac xa) xVector3
        GL.rotate (realToFrac ya) yVector3
        drawC w

    GLFW.swapBuffers
  where
    xVector3 = GL.Vector3 1 0 0 :: GL.Vector3 GL.GLfloat
    yVector3 = GL.Vector3 0 1 0 :: GL.Vector3 GL.GLfloat
    w = 0.5

drawC :: GL.GLfloat -> IO ()
drawC s = do
  GL.scale s s s
  GL.renderPrimitive GL.Quads $ do
    -- front
    GL.color red
    GL.normal (GL.Normal3 z  z  p1)
    GL.vertex (GL.Vertex3 n1 n1 p1)
    GL.vertex (GL.Vertex3 p1 n1 p1)
    GL.vertex (GL.Vertex3 p1 p1 p1)
    GL.vertex (GL.Vertex3 n1 p1 p1)
    -- back
    GL.color red
    GL.normal (GL.Normal3  z  z n1)
    GL.vertex (GL.Vertex3 n1 n1 n1)
    GL.vertex (GL.Vertex3 n1 p1 n1)
    GL.vertex (GL.Vertex3 p1 p1 n1)
    GL.vertex (GL.Vertex3 p1 n1 n1)
    -- top
    GL.color green
    GL.normal (GL.Normal3 z  p1  z)
    GL.vertex (GL.Vertex3 n1 p1 n1)
    GL.vertex (GL.Vertex3 n1 p1 p1)
    GL.vertex (GL.Vertex3 p1 p1 p1)
    GL.vertex (GL.Vertex3 p1 p1 n1)
    -- bottom
    GL.color green
    GL.normal (GL.Normal3  z n1  z)
    GL.vertex (GL.Vertex3 n1 n1 n1)
    GL.vertex (GL.Vertex3 p1 n1 n1)
    GL.vertex (GL.Vertex3 p1 n1 p1)
    GL.vertex (GL.Vertex3 n1 n1 p1)
    -- right
    GL.color blue
    GL.normal (GL.Normal3 p1  z  z)
    GL.vertex (GL.Vertex3 p1 n1 n1)
    GL.vertex (GL.Vertex3 p1 p1 n1)
    GL.vertex (GL.Vertex3 p1 p1 p1)
    GL.vertex (GL.Vertex3 p1 n1 p1)
    -- left
    GL.color blue
    GL.normal (GL.Normal3 n1  z  z)
    GL.vertex (GL.Vertex3 n1 n1 n1)
    GL.vertex (GL.Vertex3 n1 n1 p1)
    GL.vertex (GL.Vertex3 n1 p1 p1)
    GL.vertex (GL.Vertex3 n1 p1 n1)

red, green, blue :: GL.Color4 GL.GLfloat
red   = GL.Color4 1 0 0 1
green = GL.Color4 0 1 0 1
blue  = GL.Color4 0 0 1 1

z, n1, p1 :: GL.GLfloat
z  =  0
n1 = -1
p1 =  1