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

module DataSourceSettings where
       
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
  
loadDaqSettings :: Object -> Either String DataSourceSettings
loadDaqSettings obj =  do
    dataSource <- parseEither (.: "dataSource" ) obj
    daqsList   <- parseEither (.: "daqs") dataSource :: Either String Array
    outFile    <- parseEither (.: "outFile") dataSource :: Either String FileName
    let outParam = if (outFile == "none") then Nothing else Just outFile
    daqs       <- forM  (V.toList daqsList) 
                  (parseEither (\obj -> parseJSON obj)) 
    case (daqs, (parseEither (.: "inFile") dataSource))  of
      ([]  , Right fn) -> return $ DataSourceSettings (File fn) outParam
      (ds@(_:_), Left _) ->   return $ DataSourceSettings (Hardware daqs) outParam
      ([], Left s) -> Left $ "loadDaq error " ++ s
      (_:_, Right fn) -> Left "Data sources must be daqs OR file, not both."

{-  Doesn't compile (wrong number of fields?  
instance FromJSON DataSourceSettings where
  parseJSON (Object d) = DataSourceSettings <$>
                         d .: "daqs"    <*>
                         d .: "inFile"  <*>
                         d .: "outFile"
  parseJSON          _ = mzero
                         -}