{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Arte.Common.NetMessage where

import Data.Ephys.EphysDefs
import Data.Ephys.Cluster

import GHC.Generics (Generic)
import Data.Serialize
import Data.Time
import Data.Time.Clock

data ArteMessage = ArteMessage { msgTime :: ExperimentTime
                               , msgFrom :: String
                               , msgTo   :: Maybe String
                               , msgBody :: NetRequest
                               } deriving (Generic, Eq, Show)

instance Serialize ArteMessage
                                          
data NetRequest = NetPing
                | ForceQuit
                deriving (Generic,Eq, Show)

instance Serialize NetRequest

data NetResponse = EmptyResponse
                 | NetPong
                 deriving (Generic,Eq, Show)

instance Serialize NetResponse