module BackendState where

import System.ZMQ

data NetworkState = NetworkState { zmqContext :: Context
                                 , replySock :: Socket Rep 
                                 , requestSock :: Socket Req 
                                 , broadcastSock :: Socket Pub
                                 }
                    
data AcqState = AcqState { acquiring    :: Bool
                         , disking      :: Maybe Handle
                         , broadcasting :: Bool
                         }
                    
data BackendState = BackendState { net :: NetworkState
                                 , acq :: MVar AcqState
                                 }
                    
-- FIXME
initSimple :: IO BackendState
initSimple = do
  context <- zmqContext 1
  repSock <- zmqSocket Rep
  reqSock <- zmqSocket Req
  brdSock <- zmqSocket Pub
  let acqState = AcqState False Nothing False
  netState <- newMVar $ NetworkState context repSock reqSock brdSock
  BackendState { net = netState, acq = acqState }
  
-- FIXME
initFile :: Filepath -> ErrorT (IO BackendState)
initFile fn = do
  
  