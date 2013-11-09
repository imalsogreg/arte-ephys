module Arte.Common.NetMessage where

import qualified Network as N

data NetRequest = NetPing
                deriving (Eq, Show)

data NetResponse = NetPong
                 deriving (Eq, Show)
