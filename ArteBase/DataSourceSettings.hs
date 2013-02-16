----------------------------------------------------------------------
-- |
-- Module      : DataSourceSettings
-- Copyright   : (c) Greg Hale
-- License     : GPL-3
-- 
-- Maintainer  : Greg Hale <imalsogreg@gmail.com>
-- Stability   : unstable 
-- Portability :
--
---------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module DataSourceSettings (
  DataSourceSettings
  , parseJSON
  ) where
       
import Control.Applicative
import Control.Monad
import Data.Yaml
import qualified Data.Vector as V
import DaqSettings

type FileName = String

data SourceType = Hardware [DaqSettings]
                | File     FileName

data DataSourceSettings = 
  DataSourceSettings{ source      :: SourceType
                    , outputFile  :: Maybe FileName
                    }
  
{-  Doesn't compile (wrong number of fields?  
instance FromJSON DataSourceSettings where
  parseJSON (Object d) = DataSourceSettings <$>
                         d .: "daqs"    <*>
                         d .: "inFile"  <*>
                         d .: "outFile"
  parseJSON          _ = mzero
                         -}