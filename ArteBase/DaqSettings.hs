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

module DaqSettings ( 
  DaqSettings
  , parseJSON
  ) where

import           Control.Applicative
import           Control.Monad
import           Prelude
import           Data.Yaml
import qualified Data.Vector as V

type DaqName = String
type DevName = String

data DaqSettings = DaqSettings{ daqName    :: DaqName
                              , devName :: DevName
                              , nChans  :: Int
                              , nSamps  :: Int
                              , daqGains   :: [Double]
                              } deriving (Show)

instance FromJSON DaqSettings where
  parseJSON (Object d) = DaqSettings <$>
                         d  .: "name" <*>
                         d  .: "devName" <*>
                         d  .: "nChans" <*>
                         d  .: "nSamps" <*>
                         d  .: "gains"
  parseJSON _          = mzero
  

{-
loadDaqSettings :: Object -> Either String [DaqSettings]
loadDaqSettings v = 
  flip parseEither v $ \obj -> do
    dataSource <- obj .: "dataSource"
    daqs       <- (dataSource .: "daqs") :: Parser (V.Vector Object)
    return $ 
-}



{-
myTest :: IO (Either String Object) -> 
          IO (Either String [DaqSettings])
myTest the_v = do
  x <- the_v
  case x of
    Right b  -> return $ loadDaqSettings b
    Left  s  -> return (Left s)
  -}
