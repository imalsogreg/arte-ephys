----------------------------------------------------------------------
-- |
-- Module      : DaqSettings
-- Copyright   : (c) Greg Hale
-- License     : GPL-3
-- 
-- Maintainer  : Greg Hale <imalsogreg@gmail.com>
-- Stability   : unstable 
-- Portability :
--
----------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module DaqSettings where

import           Control.Applicative
import           Control.Monad
import           Prelude
import           Data.Yaml
import qualified Data.Vector as V
import           System.IO.Unsafe

type DaqName = String
type DevName = String

data DaqSettings = DaqSettings{ daqName    :: DaqName
                              , devName :: DevName
                              , nChans  :: Int
                              , nSamps  :: Int
                              , daqGains   :: Int  -- should be Vector Double
                              } deriving (Show)

instance FromJSON DaqSettings where
  parseJSON (Object d) = DaqSettings <$>
                         d  .: "name" <*>
                         d  .: "devName" <*>
                         d  .: "nChans" <*>
                         d  .: "nSamps" <*>
                         d  .: "gains"
  parseJSON _          = mzero
  
loadDaqSettings :: Object -> Either String [DaqSettings]
loadDaqSettings obj =  do
    dataSource <- parseEither (.: "dataSource" ) obj
    daqsList   <- parseEither (.: "daqs") dataSource :: Either String Array
    let a = V.toList daqsList
    daqs       <- forM  (V.toList daqsList) 
                  (parseEither (\obj -> parseJSON obj)) 
    return daqs




{-
myTest :: IO (Either String Object) -> 
          IO (Either String [DaqSettings])
myTest the_v = do
  x <- the_v
  case x of
    Right b  -> return $ loadDaqSettings b
    Left  s  -> return (Left s)
  -}
