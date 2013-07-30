----------------------------------------------------------------------
-- |
-- Module      : System.Arte.DaqSettings
-- Copyright   : (c) Greg Hale
-- License     : GPL-3
-- 
-- Maintainer  : Greg Hale <imalsogreg@gmail.com>
-- Stability   : unstable 
-- Portability :
--
----------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module System.Arte.DaqSettings where

import           Control.Applicative
import           Control.Monad
import           Prelude
import           Data.Yaml
--import qualified Data.Vector as V
--import           System.IO.Unsafe

type DaqName = String
type DevName = String

data DaqSettings = DaqSettings{ daqName  :: DaqName
                              , devName  :: DevName
                              , nChans   :: Int
                              , nSamps   :: Int
                              , daqGains :: [Double]
                              } deriving (Show)

instance FromJSON DaqSettings where
  parseJSON (Object d) = DaqSettings <$>
                         d  .: "name" <*>
                         d  .: "devName" <*>
                         d  .: "nChans" <*>
                         d  .: "nSamps" <*>
                         d  .: "gains"
  parseJSON _          = mzero
  

