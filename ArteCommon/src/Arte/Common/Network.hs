{-# LANGUAGE OverloadedStrings #-}

module Arte.Common.Network where

import Data.Text hiding (unwords)
import Data.Maybe (catMaybes)
import Control.Applicative
import Control.Monad
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Yaml
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans.Class
import qualified Data.HashMap.Strict as HashMap

import Data.Maybe

type HostName = String
type IPAddy   = String
type Port     = String
{-
getAppNode :: String -> String -> FilePath -> IO (Either String Node)
getAppNode nodeType appName fn = do
  f <- BS.readFile fn
  let v =  Data.Yaml.decode f :: Maybe Value
  case v ^. key (pack nodeType) :: Maybe [Value] of
    Nothing -> return $ Left ("No node type " ++ nodeType)
    Just nodes ->
      case Prelude.filter (\n -> nodeName n == (pack appName)) nodes of
        [] -> return $ Left (unwords ["Couldn't find",appName,"in",nodeType])
        (m:atches) -> return $ Right m
-}

a :: Node
a = Node "sampleNodeName" (Host "HostName" "HostIP") "nodePort"

getF :: IO BS.ByteString
getF = BS.readFile "/home/greghale/.arte-ephys/network.conf"

getV :: IO (Maybe Value)
getV = Data.Yaml.decode `liftM` getF

getObjs :: IO [Value]
getObjs = do
  v <- getV
  return $ v ^. key "spikes" . folded :: IO [Value]

data Host = Host 
            { hostName :: String
            , hostIP   :: String
            } deriving (Eq, Show)
                       
data Node = Node 
            {  nodeName :: String
            ,  nodeHost :: Host
            ,  nodePort :: String
            } deriving (Eq, Show)

instance FromJSON Host where
  parseJSON (Object v) = Host <$> 
                         v .: "name" <*>
                         v .: "ip"
  parseJSON _  = mzero
  
instance FromJSON Node where
  parseJSON (Object v) = Node <$>
                         v .: "name" <*>
                         v .: "host" <*>
                         v .: "port"
  parseJSON _          = mzero

instance ToJSON Host where
  toJSON (Host hName hIP) =
    object ["name" .= hName
           ,"ip"   .= hIP
           ]

instance ToJSON Node where
  toJSON (Node nName nHost nPort) =
    object ["name" .= nName
           ,"host" .= nHost
           ,"port" .= nPort]
