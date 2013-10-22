{-# LANGUAGE OverloadedStrings #-}

module Arte.Common.Network where

import Data.Text hiding (unwords)
import Control.Applicative
import Control.Monad
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Yaml
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map, keys, member)

type HostName = String
type IPAddy   = String
type Port     = String

getAppNode :: String -> FilePath -> IO (Either String Node)
getAppNode nodeName fn = do
  f <- BS.readFile fn
  let v =  Data.Yaml.decode f :: Maybe Value
  case v ^. key "nodes" . key (pack nodeName) :: Maybe Node of
    Just node -> return $ Right node

  -- Error handling
    Nothing->
      case v of
        Nothing -> return $ Left ("Bad conf file: " ++ fn)
        _ -> case v ^. key "nodes" :: Maybe (Map String Node) of
          Nothing      ->
            return $ Left ("Malformed conf file lacks nodes: field")
          Just nodeMap -> case member nodeName nodeMap of
            True  -> return $ Left ("No parse for node " ++ nodeName)
            False -> return $ Left
                     ("Node '" ++ nodeName ++ "' not in node list: " ++
                      (unwords . keys  $ nodeMap))

data Host = Host 
            { hostIP   :: String
            } deriving (Eq, Show)
                       
data Node = Node 
            {  nodeHost :: Host
            ,  nodePort :: Int
            } deriving (Eq, Show)

instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "ip"
  parseJSON _  = mzero
  
instance FromJSON Node where
  parseJSON (Object v) = Node <$>
                         v .: "host" <*>
                         v .: "port"
  parseJSON _          = mzero

instance ToJSON Host where
  toJSON (Host hIP) =
    object ["ip"   .= hIP]

instance ToJSON Node where
  toJSON (Node nHost nPort) =
    object ["host" .= nHost
           ,"port" .= nPort]
