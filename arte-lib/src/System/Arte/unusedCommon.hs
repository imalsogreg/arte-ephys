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

module Arte.Common (
  module Arte.Common.FileUtils,               
  module Arte.Common.Net,
  module Arte.Common.NetMessage
  ) where

import Arte.Common.FileUtils
import Arte.Common.Net
import Arte.Common.NetMessage

                 
{- This is defined in haskell-tetrode-ephys
   as ExperimentTime = Double
type TimeStamp = Double
-}            

{- Move these to NetMessage.hs

data ArteMessage = ArteMessage TimeStamp ArteCommand
                   deriving (Show)
  
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
-}                  

data VerbosityLevel = VerbositySilent
                    | VerbosityErrors
                    | VerbosityWarnings
                    | VerbosityNotices
                    | VerbosityEverything
                    | VerbositySpecial -- For debugging
                      deriving (Eq, Show, Enum, Ord)
                               
{- Old attempt                               
connectToMasterServer :: Context -> ZmqSockStr -> IO (Socket Req)
connectToMasterServer ctx masterStr = do
  cliSocket <- socket ctx Req
  connect cliSocket masterStr
  return cliSocket
  -}

  