{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Ephys.Timeseries.Types where

import Data.Map
import Data.Vector
import Control.Lens
import Data.Text
import GHC.Generics

-- |A timeseries has a 
data Timeseries a = Timeseries { _tStart :: Double
                               , _tEnd   :: Double
                               , _tsData   :: Map Text (Vector a)
                               }
                  deriving (Eq, Generic)

$(makeLenses ''Timeseries)
