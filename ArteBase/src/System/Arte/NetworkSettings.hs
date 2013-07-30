module System.Arte.NetworkSettings where

import Data.Text
import Control.Applicative
import Control.Monad
import Data.Yaml

data Host = Host 
            { hostName :: String
            , hostIP   :: String
            } deriving (Show)
                       
data Node = Node 
            {  nodeName :: String
            ,  nodeHost :: Host
            ,  nodePort :: String
            }

data NetworkSettings = NetworkSettings {
    networkHosts :: [Host]
  , networkNodes :: [Node]
  }


instance FromJSON Host where
  parseJSON (Object v) = Host <$> 
                         v .: pack "name" <*>
                         v .: pack "ip"
  parseJSON _  = mzero
  
instance FromJSON Node where
  parseJSON (Object v) = Node <$>
                         v .: pack "name" <*>
                         v .: pack "host" <*>
                         v .: pack "port"
  parseJSON _          = mzero


                       
instance FromJSON NetworkSettings where
  parseJSON (Object v) = NetworkSettings <$>
                         v .: pack "hosts" <*>
                         v .: pack "nodes"
  parseJSON _          = mzero
  