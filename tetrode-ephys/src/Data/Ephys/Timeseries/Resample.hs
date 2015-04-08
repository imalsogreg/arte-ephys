{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Ephys.Timeseries.Resample where

import Data.Ephys.Timeseries.Types

import Data.Map
import Safe
import GHC.Generics

data PaddingRule = PaddingClip | PaddingZero
                 deriving (Eq, Show, Generic)

data ShiftRule   = ShiftInterp | ShiftNearest | ShiftMostRecent
                 deriving (Eq, Show, Generic)

samplingRate :: Timeseries a -> Maybe Int
samplingRate ts =
  length (ts^.tDtata) (ts^.tEnd - ts^.tStart)

inInterval :: Double -> Timeseries a -> Bool
inInterval t ts = t < ts^.tsTStart || t > ts.tsTEnd

tsLength :: Timeseries a -> Double
tsLength ts = ts^.tsInterval.snd - ts^.tsInterval-fst

tsTIndInt :: Timeseries a -> Double -> Maybe Int
tsTIndInt ts t
  | not (inInterval t ts) = Nothing
  | otherwise = floor $ (t - ts^.tsTStart)/(intervalLength ts)

tsIndT :: Timeseries a -> Int -> Double
tsIndT ts i = ts^.tStart + fromIntegral i / samplingRate ts

sample :: (Real a) => ShiftRule
          -> Double
          -> Timeseries a
          -> a
sample padR shiftR t ts
  | not (inInterval t ts) = 0
  | otherwise = undefined

subWindow :: (Real a) =>
             PaddingRule ->
             ShiftRule ->
             Double ->
             Double ->
             Timeseries a ->
             Timeseries a
subWindow padR shiftR tStart tEnd ts =
  