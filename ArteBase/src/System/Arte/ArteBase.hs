----------------------------------------------------------------------
-- |
-- Module      : Arte.Base
-- Copyright   : (c) Greg Hale
-- License     : GPL-3
-- 
-- Maintainer  : Greg Hale <imalsogreg@gmail.com>
-- Stability   : unstable 
-- Portability :
--
-- Arte.Base A module containing base values for all arte programs
--
----------------------------------------------------------------------

module ArteBase where

import System.ZMQ
import ZmqUtils


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
  
    