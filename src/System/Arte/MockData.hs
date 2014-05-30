{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, TemplateHaskell #-}

module Main where

import Data.Ephys.EphysDefs
import Data.Ephys.Spike
import Data.Ephys.Cluster
import Data.Ephys.Position
import Data.Ephys.TrackPosition
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.Parse
import Data.Ephys.OldMWL.ParseClusterFile
import Data.Ephys.OldMWL.ParsePFile
import System.Arte.Net
import System.Arte.FileUtils
import System.Arte.DataPublisher
import System.Arte.CommandPort

import Pipes.RealTime

import System.IO
import Network
import Control.Applicative
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Time.Clock
import Pipes ( (>->), lift )
import qualified Pipes as P
import qualified Pipes.Prelude as PP
import System.Directory
import System.FilePath ((</>))
import Control.Monad.Trans.Writer.Strict
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.Async
--import Control.Concurrent.STM.TQueue
import Control.Monad
import Data.Text (Text,pack,unpack)
import qualified Data.Map as M
import qualified Data.Serialize as S
import Control.Lens

data MockStatus = Seeking | Waiting | Streaming
                deriving (Eq, Show)

data DataFormat = JSON | Binary
                deriving (Eq, Show)

data DataSource a = DataSource {
    _dsProducers :: [P.Producer a IO ()]
  , _dsWaiting   :: TVar (M.Map ExperimentTime a)
  , _dsGetTS     :: (a -> ExperimentTime)
  , _dsPub       :: DataPublisher a
  , _dsNode      :: Node
  , _dsFormat    :: DataFormat
  }
$(makeLenses ''DataSource)

data MockState = MockState
                 { _mockStatus       :: TVar MockStatus
                 , _mockWallTimeFun  :: TVar (UTCTime -> ExperimentTime)
                 , _mockSpikeSources :: [DataSource TrodeSpike]
                 , _mockPosSources   :: [DataSource Position]
--                 , _mockLFPSources   :: [DataSource Lfp]
                 , _mockNode         :: Node
                 , _readAheadSec     :: ExperimentTime
                 , _waitAfterSeek    :: Bool
                 , _defaultSeekPoint :: Double
                 }
$(makeLenses ''MockState)

mkSource :: [P.Producer a IO ()] -> (a -> ExperimentTime) -> Node ->
            DataFormat -> ExperimentTime -> 
            IO (DataSource a)
mkSource producers timeFun node format tTarget = do
  q <- atomically . newTVar $ M.empty
  s <- atomically . newTVar $ Seeking
  p <- DataPublisher <$> (atomically $ newTQueue) <*> newTVarIO []
  return $ DataSource (seq prods' prods') q timeFun p node format
    where prods' = map (>-> PP.dropWhile (\a -> timeFun a < tTarget)) producers

------------------------------------------------------------------------------
runDataSource :: MockState -> DataSource a -> IO ()
runDataSource opts ds = do
  inThread  <- async $ (atomically $ readTVar (ds^.mockStatus) >>= \r ->
                         unless (r == Waiting) retry)
               >> runInput
  outThread <- async $ runOutput
  wait [inThread,outThread]
    where
      runInput = do
        tTarget <- spoolToTime
        forM (ds^.dsProducers) $ \p ->
          P.for (p >-> PP.takeWhile (\a -> (ds^.dsGetTS $ a) <= tTarget))
          (\a -> atomically $ M.insert (ds^.dsGetTS $ a) a (ds^.dsWaiting))

      runOutput = P.for (outProducer >-> relativeTimeCat (ds^.dsGetTS))
                  (atomically $ writeTQueue (ds^.dsPub.chan))
      outProducer = forever $ do
        v <- atomically $ do
          m <- readTVar $ ds^.dsWaiting 
          case M.lookupIndex 0 m of
            Nothing    -> retry
            Just (k,v) -> return k
        yield v

      spoolToTime :: IO ExperimentTime
      spoolToTime = do
        f <- atomically $ readTVar ds^.mockWallTimeFun
        t <- getCurrentTime
        return $ f t + opts^.readAheadSec
      
            
  

--TODO Make mockData work on general tracks
track :: Track
track = circularTrack (0,0) 0.57 0.5 0.25 0.15

main :: IO ()
main = do
 undefined

{-
queueToNetwork :: (S.Serialize a, Show a) => Bool -> TQueue a -> Node -> IO ()
queueToNetwork verbose q node = do
  let portStr = zmqStr Tcp "*" (show $node ^. port)
  ZMQ.withContext 1 $ \ctx -> do
    ZMQ.withSocket ctx ZMQ.Pub $ \pubSock -> do
      ZMQ.bind pubSock portStr
      forever $ do
        a <- atomically $ readTQueue q
        when verbose $ print a
        ZMQ.send pubSock (S.encode a) []

setupPub :: Node -> IO (ZMQ.Socket ZMQ.Pub, MVar ())
setupPub node = do
  let portStr = zmqStr Tcp "*" (show $ node^.port)
  ZMQ.withContext 1 $ \ctx -> do
    ZMQ.withSocket ctx ZMQ.Pub $ \pubSock -> do
      ZMQ.bind pubSock portStr
      lock <- newEmptyMVar
      return (pubSock, lock)

toNetwork :: (S.Serialize a, Show a) => Bool -> ZMQ.Socket ZMQ.Pub -> MVar ()
             -> a -> IO ()
toNetwork verbose pubSock lock a = do
  withMVar lock $ \_ -> do
    when verbose $ print a
    ZMQ.send pubSock (S.encode a) []
-}

{- These have been moved to haskel-tetrode-ephys
-- "path/to/0224.tt" -> "24"
-- TODO : Fix.  Only drop 5 when extention has 2 letters.
mwlTrodeNameFromPath :: String -> Text
mwlTrodeNameFromPath = pack . reverse . take 2 . drop 5 . reverse  

-- "path/to/0224.tt" -> "path/to/cbfile-run"
cbNameFromTTPath :: String -> FilePath -> Text
cbNameFromTTPath newName ttPath = pack . (++ newName) .
                              reverse . dropWhile (/= '/') . reverse $ ttPath
-}


pipeToQueue :: Bool -> TQueue a -> P.Consumer a IO r
pipeToQueue verbose q = forever $ do
  s <- P.await
  when verbose (P.liftIO . print $ "Pipe to Queue")
  P.lift . atomically $ writeTQueue q s

dropWhile' :: (Monad m) => (a -> Bool) -> P.Pipe a a m ()
dropWhile' p = do
  v <- P.await
  case p v of
    True  -> dropWhile' p
    False -> P.yield v

dropWhile'' :: (Monad m) => (a -> Bool) -> P.Pipe a a m ()
dropWhile'' _ = P.cat

seekAndWait :: (Show a) => TMVar () -> (a -> Double) -> Double -> P.Pipe a a IO () -> P.Pipe a a IO ()
seekAndWait goSignal toTime target produce = do
  dropWhile' ((< target) . toTime)
  lift $ print "Seek and wait ready."
  () <- lift . atomically . readTMVar $ goSignal  -- Block here
  produce

-- TODO: Try this out
{- dropWhile' + seekAndWait leaks one value out right after pausing for the signal.  To avoid this, maybe:
seekAndWait :: (Show a) => TMVar () -> (a -> Double) -> Double -> P.Pipe a a IO () -> P.Pipe a a IO ()
seekAndWait goSignal toTime target produce = do
  v <- P.await
  pase p v of
    True -> seekAndWait p goSignal toTime target produce
    False -> do
      () <- lift . atomically . readTMVar $ goSignal -- Block here
      yield v                                        -- Release the first non-drop value
      produce                                        -- Continue producing

-}

{-
orderClusters :: TQueue ArteMessage -> FilePath -> FilePath -> IO ()
orderClusters queue cFile ttFile = do 
  let trodeName = unpack $ mwlTrodeNameFromPath ttFile
  cExists <- doesFileExist cFile
  when cExists $ do
    clusters' <- getClusters cFile ttFile
    case clusters' of
      Left _         -> return ()
      Right clusts -> atomically . writeTQueue queue $
                      (ArteMessage 0 "" Nothing (Request $ TrodeSetAllClusters (read trodeName) clusts))
  -- TODO: Unsafe use of read!!

main :: IO ()
main = do
  
  opts <- cmdArgs mockCmd
  --print opts
  let spikeExt = spikeFileExtension opts
      eegExt   = eegFileExtention opts
      pExt     = pFileExtention opts
      cbName   = clusterBoundsFileName opts
      arteExt = arteFileExtension opts
      
      sDepth = searchDepth opts
      baseDir = baseDirectory opts
  
  [spikeFiles,eegFiles,pFiles,arteFiles] <- mapM (getFilesByExtension baseDir sDepth)
                                  [spikeExt,eegExt,pExt,arteExt]
  
  nodes <- mapM (flip getAppNode Nothing) ["spikesA","lfpsA","pos","master"]
  case nodes of
    [Right spikeNode, Right eegNode, Right pNode, Right masterNode] -> do

{-  This method didn't work.  "Bad file descriptor" prob b/c different ZMQ thread
    is different from spike-writing thread
      print "ABOUT TO SETUP"
      (spikeSock,spikeLock) <- setupPub spikeNode
      print "FINISHED SETUP"
-}

      spikeQ <- newTQueueIO
      eegQ   <- newTQueueIO
      posQ   <- newTQueueIO      
      goSign <- newEmptyTMVarIO 

      withMaster masterNode $ \(toMaster,fromMaster) -> do

        print "About to hondle events"
        forkFinally (handleEvents fromMaster goSign) (\_ -> print "Handle Events forkFinally")

        spikeAsyncs <- forM spikeFiles $ \fn -> do
          let tName = read . unpack $ mwlTrodeNameFromPath fn
              cName = cbNameFromTTPath (clusterBoundsFileName opts) fn
          fi' <- getFileInfo fn
          f  <- BSL.readFile fn
          case fi' of
            Left e -> error $ "Bad fileinfo for file " ++ fn ++ " error: " ++ e
            Right fi -> do
              print "ORDER CLUSTERS"
              orderClusters toMaster (unpack cName) fn
              print "DID AN ORDER CLUSTERS"
              async .P.runEffect $ (dropResult $ produceTrodeSpikes tName fi f) >->
                seekAndWait goSign spikeTime (startExperimentTime opts)
                (relativeTimeCat (\s -> (spikeTime s - startExperimentTime opts))) >-> 
--                PP.chain (\thisSpike -> print $ spikeTrodeName thisSpike) >->
                pipeToQueue False spikeQ
--                (\spike -> lift $ toNetwork False spikeSock spikeLock spike)

        let p0 = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0
                 ConfSure sZ sZ (-1/0 :: Double) (Location 0 0 0) --TODO p0 is ConfSure? Test this
            sZ = take 15 (repeat 0)  --TODO customizable smoothing length, or even pipe-filter?  
            ((pX0,pY0),pixPerM,h) = ((166,140),156.6, 0.5) :: ((Double,Double),Double,Double) --TODO Right type?
        posF <- BSL.readFile $ head pFiles -- TODO REMOVE PARTIAL FUNCTION
        putStrLn $ "About to Pos Async from file " ++ head pFiles
        posAsync <- async . P.runEffect $ dropResult (produceMWLPos posF) >->
                    runningPosition (pX0,pY0) pixPerM h p0 >->
                    seekAndWait goSign _posTime (startExperimentTime opts)
                    (relativeTimeCat (\p -> (_posTime p - startExperimentTime opts))) >->
--                    PP.print >->
                    pipeToQueue False posQ

        _ <- async $ queueToNetwork False spikeQ spikeNode
        _ <- async $ queueToNetwork False (posQ :: TQueue Position)     pNode

        _ <- mapM waitCatch spikeAsyncs
        wait posAsync

        print "Finished spooling data"

    _ -> error $ "Problem loading configuration data."

handleEvents :: TQueue ArteMessage -> TMVar () -> IO ()
handleEvents sub goSign = forever $ do
  m <- atomically $ readTQueue sub
  case msgBody m of
    Request StartAcquisition -> putStrLn "Got Go signal!" >> atomically (putTMVar goSign ())
    _ -> putStrLn $ "Got and ignored a message: " ++ (take 40 . show . msgBody $ m)
-}


{-
acceptDataClients :: Node -> IO ()
acceptDataClients node = case node^.port of
  Nothing -> error "Configuration error: data generating node has no port field"
  Just p -> do
    server <- DataServer <$> newTVarIO []
    print $ "Awaiting data connections, listening on " ++ show p
    sock <- listenOn (PortNumber . fromIntegral $ p)
    loop sock server
      where
        loop s server = do
          (handle,clientHost,clientPort) <- accept s
          client <- DataClient <$> newTQueueIO <*> pure handle
          atomically $ modifyTVar (clients server) (client :)
          putStrLn $ unwords ["Accepted host", clientHost, "on port", show clientPort]
          forkIO $ runClientHandler client
          loop s server

data DataServer = DataServer { clients :: [DataClient] }

data DataClient = DataClient { dataToClient :: TQueue TrodeSpike
                             , clientHandle :: Handle
                             }

dataToServer :: (S.Serialize a, Show a) => DataServer -> a -> IO ()
dataToServer server a =
  atomically $ forM_ (clients server) (writeTQueue a)
  
runClientHandler :: DataClient -> IO ()
runClientHandler client =
  forever $ do
    a <- readTQueue (dataToClient client)
    sendWithSize $ (encode a) (clientHandle client)
                     
-}
