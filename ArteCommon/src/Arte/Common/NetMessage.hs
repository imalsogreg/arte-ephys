{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Arte.Common.NetMessage where

import Data.Serialize
import GHC.Generics (Generic)

import qualified Network as N

data NetRequest = NetPing
                deriving (Generic,Eq, Show)

instance Serialize NetRequest

data NetResponse = NetPong
                 deriving (Generic,Eq, Show)

instance Serialize NetResponse