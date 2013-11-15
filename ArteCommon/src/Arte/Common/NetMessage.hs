{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Arte.Common.NetMessage where

import Data.Ephys.EphysDefs
import Data.Ephys.Cluster

import GHC.Generics (Generic)
import Data.Serialize
import Data.Time
import Data.Time.Clock
import qualified Data.Map as Map

data ArteMessage = ArteMessage { msgTime :: ExperimentTime
                               , msgFrom :: String
                               , msgTo   :: Maybe String
                               , msgBody :: MessageBody
                               } deriving (Generic, Eq, Show)

instance Serialize ArteMessage

data MessageBody = Request  NetRequest
                 | Response NetResponse
                 deriving (Generic, Eq, Show)

instance Serialize MessageBody

data NetRequest = NetPing
                | ServerHangup
                | ForceQuit
                | SetAllClusters String (Map.Map Int ClusterMethod)
                | StartAcquisition
                deriving (Generic,Eq, Show)

instance Serialize NetRequest

data NetResponse = EmptyResponse
                 | NetPong
                 deriving (Generic,Eq, Show)

instance Serialize NetResponse