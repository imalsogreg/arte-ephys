{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Arte.Common.NetMessage where

import Data.Ephys.Cluster

import GHC.Generics (Generic)
import Data.Serialize
import Data.Time

data ArteMessage = ArteMessage { msgTime :: UTCTime
                               , msgFrom :: String
                               , msgTo   :: Maybe String
                               , msgBody :: NetRequest
                               } deriving (Generic, Eq, Show)
  
data NetRequest = NetPing
                deriving (Generic,Eq, Show)

instance Serialize NetRequest

data NetResponse = NetPong
                 deriving (Generic,Eq, Show)

instance Serialize NetResponse