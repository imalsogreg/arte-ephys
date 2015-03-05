module Main where

import qualified Data.Array.Repa as R
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Game as G

import Kalman

type Field a = R.Array R.U R.DIM2 a

data Model = Model { field :: Field Float }

main :: IO ()
main = G.playIO dispMode G.white 30 world0 draw action step

dispMode :: G.Display
dispMode = G.InWindow "Window" (640,480) (10,10)

--world0 :: Model
--world0 = Model $ R.computeP (fromFunction (Z :. 100 :. 100)
--                             (\(
