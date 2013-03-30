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

import ZmqUtils

{-
type ArteReq = ArteReqSock ZmqSockStr
type ArteRep = ArteRepSock ZmqSockStr
type ArtePub = ArtePubSock [ZmqSockStr]
type ArteSub = ArteSubSock [ZmqSockStr]

-- Description of the types of zmq sockets used throughout arte
-- One confusing part - is this the right type for ArteReq?
-- What about server that has many clients?
-- ie - most servers will have only one client (master as client)
--      but master itself will have several clients w/ different sockStrs..
data ArteSock = ArteReq ZmqSockStr
              | ArteRep ZmqSockStr
              | ArtePub [ZmqSockStr]
              | ArteSub [ZmqSockStr]

-- A data type for each sort of node
-- Is it right?  Seems like a lot of boilerplate
-- What I want to achieve is that the types enforce the 
-- sorts of sockets each node has
-- Something like Master only has in/out as rep/req, but
-- Backend is a data source (has dataOut, no dataIn),
-- and SpikeViewer (which may also be a spike-sorter) has
-- both dataOut and dataIn...  Is it actually helpful to have
-- this in the types?
data BackendNode = BackendNode { masterIn  :: ArteRep ZmqSockStr
                               , masterOut :: ArteReq ZmqSockStr
                               , dataOut   :: ArtePub [ZmqSockStr]
                               }

data SpikeViewerNode = SpikeViewerNode { masterIn  :: ArteRep ZmqSockStr
                                       , masterOut :: ArteReq ZmqSockStr
                                       , dataIn    :: ArteSub [ZmqSockStr]
                                       , dataOut   :: ArtePub [ZmqSockStr]
                                       }
                       
data LFPViewer = LFPViewer { masterIn  :: ArteRep ZmqSockStr
                           , masterOut :: ArteReq ZmqSockStr
                           , dataIn    :: ArteSub [ZmqSockStr]
                           , dataOut   :: ArtePub [ZmqSockStr]
                           }

data Tracker = Tracker { masterIn  :: ArteRep ZmqSockStr
                       , masterOut :: ArteReq ZmqSockStr
                       , dataOut   :: ArtePub [ZmqSockStr]
                       }

data Master = Master { masterOut :: ArteReq ZmqSockStr
                     , masterIn  :: ArteRep ZmqSockStr
                     }

-- ArteNode is an arte executable, parameterized
-- on the 
data ArteNode = Backend Rep
          | Master OutputPort
          | SpikeViewer
          | LFPViewer
          | Tracker
-}            

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