{-# LANGUAGE OverloadedStrings #-}

module System.Arte.NetworkSettings where

import Data.Text
import Control.Applicative
import Control.Monad
import Data.Yaml

data Host = Host 
            { hostName :: Text
            , hostIP   :: String
            } deriving (Show)
                       
data Node = Node 
            {  nodeName :: Text
            ,  nodeHost :: Host
            ,  nodePort :: String
            }

instance FromJSON Host where
  parseJSON (Object v) = Host <$> 
                         v .: "name" <*>
                         v .: "ip"
  parseJSON _  = mzero
  
instance FromJSON Node where
  parseJSON (Object v) = Node <$>
                         v .: pack "name" <*>
                         v .: pack "host" <*>
                         v .: pack "port"
  parseJSON _          = mzero