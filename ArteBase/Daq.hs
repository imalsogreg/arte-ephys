----------------------------------------------------------------------
-- |
-- Module      : Daq
-- Copyright   : (c) Greg Hale
-- License     : GPL-3
-- 
-- Maintainer  : Greg Hale <imalsogreg@gmail.com>
-- Stability   : unstable 
-- Portability :
--
----------------------------------------------------------------------



module Daq ( 
  Daq
  , parseJSON
  )
  where

import Control.Applicative
import Control.Monad
import Prelude
import Data.Yaml
import Data.Text (pack)

type DaqName = String
type DevName = String

data Daq = Daq{ daqName    :: DaqName
              , devName :: DevName
              , nChans  :: Int
              , nSamps  :: Int
              , daqGains   :: [Double]
              } deriving (Show)

instance FromJSON Daq where
  parseJSON (Object d) = Daq <$>
                         d  .: pack "name" <*>
                         d  .: pack "devName" <*>
                         d  .: pack "nChans" <*>
                         d  .: pack "nSamps" <*>
                         d  .: pack "gains"
  parseJSON _          = return (Daq  "name" "dev" 32 22 [])