{-|
Module      : Data.Ephys.EphysDefs
Description : Definitions of basic types
Copyright   : (c) 2015 Greg Hale, Shea Levy
License     : BSD3
Maintainer  : imalsogreg@gmail.com
Stability   : experimental
Portability : GHC, Linux
-}
{-# LANGUAGE BangPatterns, TypeSynonymInstances, DeriveDataTypeable #-}

module Data.Ephys.EphysDefs where

-- | Voltage in volts
type Voltage = Double

-- | Identifier of a place cell
type PlaceCellName = Int
-- | Identifier of a trode
type TrodeName = Int

-- | Time elapsed in the experiment
type ExperimentTime = Double -- TODO: How to record the wall clock time of "0 :: ExperimentTime"
-- Is 0 the time that the system was turned on?  Clock reset?
-- How to ensure that data that straddle a clock reset aren't combined?
-- Possibly ExperimentTime should be (StartUpUTCTime, ClockResetsSinceStartup, FloatTimeSinceReset)
