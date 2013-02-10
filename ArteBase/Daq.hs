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

import           Control.Applicative
import           Control.Monad
import           Prelude
import           Data.Yaml
import           Data.Text (pack)
import qualified Data.Vector as V

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
  parseJSON _          = return (Daq  "errorDaq" "dev" 1 1 [])
  
  
{-
daqsFromYaml :: Data.Yaml.Value -> [Daq]
daqsFromYaml val = filter (/= Nothing) mList where
  mList = V.fromList $ V.map maybeDaqFromYaml val
  maybeDaqFromYaml :: Value -> (Maybe Daq)
  maybeDaqFromYaml (Object v) = parseMaybe (.: pack ")
  daqsYaml <- val
  case parseMaybe (.: pack "daqs") c of
    Just (Array ds) -> V.toList $ V.map parseJSON ds
    Nothing -> []
daqsFromYaml Nothing = []
-}

yamlToMaybeDaqs :: Maybe Value -> [Maybe Daq]
yamlToMaybeDaqs (Maybe Array a)
