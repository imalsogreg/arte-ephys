module System.Arte.ZmqUtils where

type ZmqHost    = String
type ZmqPort    = String
type ZmqSockStr = String
data ZmqProtocol = Tcp | Ipc deriving (Eq)

instance Show ZmqProtocol where
  show Tcp = "tcp"
  show Ipc = "ipc"

zmqStr :: ZmqProtocol -> ZmqHost -> ZmqPort -> ZmqSockStr
zmqStr prot host port = 
  show prot ++ "://" ++ host ++ ":" ++ port
  