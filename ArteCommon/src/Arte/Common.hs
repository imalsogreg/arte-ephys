----------------------------------------------------------------------
-- |
-- Module      : Arte.Common
-- Copyright   : (c) Greg Hale
-- License     : GPL-3
-- 
-- Maintainer  : Greg Hale <imalsogreg@gmail.com>
-- Stability   : unstable 
-- Portability :
--
-- Arte.Common A module containing base values for all arte programs
--
----------------------------------------------------------------------

module Arte.Common where

import System.ZMQ

data ArteMessage = ArteMessage TimeStamp ArteCommand
                   deriving (Show)
                   
type TimeStamp = Double
            
data ArteCommand = StartAcq
                 | StopAcq
                 | StartDisk
                 | StopDisk
                 | ResetClocks
                 | GetActive
                 | Heartbeat
                 | SetBackendSetupConfig
                 | SetBackendSessionConfig
                 | SetTrodeConfig
                 | SetLfpConfig
                 | SetVerbosityLevel VerbosityLevel
                 | Ping String
                 | Pong String
                   deriving (Show)
                  
data VerbosityLevel = VerbositySilent
                    | VerbosityErrors
                    | VerbosityWarnings
                    | VerbosityNotices
                    | VerbosityEverything
                    | VerbositySpecial -- For debugging
                      deriving (Eq, Show, Enum, Ord)
                               
                               
connectToMasterServer :: Context -> ZmqSockStr -> IO (Socket Req)
connectToMasterServer ctx masterStr = do
  cliSocket <- socket ctx Req
  connect cliSocket masterStr
  return cliSocket
  


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
  