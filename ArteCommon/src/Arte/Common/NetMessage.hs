module Arte.Common.NetMessage where

import qualified Network as N

data MasterRequest = MRPing
                   deriving (Eq, Show)

data ClientResponse = CRPong
                    deriving (Eq, Show)
