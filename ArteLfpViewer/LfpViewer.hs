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

main :: IO ()
main = do
  _ <- GLFW.initialize
  GLFW.openWindow GLFW.defaultDisplayOptions
      { GLFW.displayOptions_numRedBits   = 8
      , GLFW.displayOptions_numGreenBits = 8
      , GLFW.displayOptions_numBlueBits  = 8
      , GLFW.displayOptions_numDepthBits = 1
      }
  return ()