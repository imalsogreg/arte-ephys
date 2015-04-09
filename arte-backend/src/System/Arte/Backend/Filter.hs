----------------------------------------------------------------------
-- |
-- Module     : Filter
-- Copyright  : (c) Greg Hale 2013
-- License    : GPL-3
-- 
-- Maintainer : imalsogreg@gmail.com
-- Stability  : unstable
-- Portability: not portable, uses posix & linux kernel modules
--
-- Filter, SOS-IIR and FIR filters
--
----------------------------------------------------------------------

module Filter where

import Data.Sequence

type FilterState a = 