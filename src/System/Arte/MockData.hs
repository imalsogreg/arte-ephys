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
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.Async
import Control.Monad.Trans.Either
import Control.Error
import Control.Monad.Trans.Writer.Strict
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Configurator as Conf
import qualified Data.Configurator.Types as Conf
import Data.Time.Clock
import Pipes ( (>->), lift )
import qualified Pipes as P
import qualified Pipes.Prelude as PP
import System.Directory
import System.FilePath ((</>))

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

data InputFileType = MwlEEG | MwlTT | MwlP

data DataSourceOpts = DataSourceOpts {
    _dsNode      :: Node
  , _dsFormat    :: DataFormat
  , _dsFilePaths :: [FilePath]
  , _dsFileType  :: InputFileType
  }
$(makeLenses ''DataSourceOpts)

data DataSource a = DataSource {
    _dsProducers :: [P.Producer a IO ()]
  , _dsWaiting   :: TVar (M.Map ExperimentTime a)
  , _dsPub       :: DataPublisher a
  , _dsGetTS  :: (a -> ExperimentTime)
  }
$(makeLenses ''DataSource)

data MockOpts = MockOpts {
    _mockNode          :: Node
  , _mockReadAheadTime :: ExperimentTime
  , _mockWaitAfterSeek :: Bool
  , _mockFirstSeekTime :: ExperimentTime
  } deriving (Eq, Show)
$(makeLenses ''MockOpts)

data MockState = MockState
                 { _mockStatus       :: TVar MockStatus
                 , _mockWallTimeFun  :: TVar (UTCTime -> ExperimentTime)
                 , _mockSpikeSources :: [DataSource TrodeSpike]
                 , _mockPosSources   :: [DataSource Position]
--                 , _mockLFPSources   :: [DataSource Lfp]
                 }
$(makeLenses ''MockState)

------------------------------------------------------------------------------
getSourceOpts :: Conf.Config -> IO [DataSourceOpts]
getSourceOpts conf = catMaybes <$> forM [0..9] $ \i -> 
  runMaybeT $ do
    let prefix = (("source" ++ show i ++ ".") ++)
    dp    <- Conf.lookup conf $ prefix "dataPort"
    let node = Node (Host prefix "localhost")
               (PortNumber . fromIntegral . read $ dp)
    files <- Conf.lookup conf $ prefix "files"
    fType <- Conf.lookup conf $ prefix "fileType"
    fmt   <- Conf.lookup conf $ prefix "outFormat"
    fp    <- lift $ processPath  files
    return $ DataSourceOpts node fmt fp  fType

getMockOpts :: FilePath -> IO (Either String MockState)
getMockOpts f = do
  config <- Conf.load [Conf.Required f]
  runEitherT $ do
    seekWait  <- Conf.lookup config "waitAfterSeeking"
    lookAhead <- lookup config "readAheadSeconds"
    basePath  <- lookup config "dataBasePath"
    cmdPort   <- lookup config "commandPort"    
    return $ MockOpts (Node (Host "MockData" "localhost")
                       (PortNumber . fromIntegral . read $ cmdPort))

main :: IO ()
main = do
  undefined



------------------------------------------------------------------------------
mkSource :: MockOpts -> DataSourceOpts -> IO (DataSource a)
mkSource mock source = do
  q <- atomically . newTVar $ M.empty
  p <- DataPublisher <$> (atomically $ newTQueue) <*> newTVarIO []
  let timeFun = case (source^.dsFileType) of
        MwlTT -> spikeTime
        MwlP  -> posTime
      node = source ^. dsNode
      format = Main.Binary
      tTarg = mock ^. mockFirstSeekTime
      producecs = 
      prods' = map (>-> dropWhile (\a -> timeFun a < tTarg)) producers
  return $ DataSource (seq prods' prods') q timeFun p node format


------------------------------------------------------------------------------
runDataSource :: MockState -> DataSource a -> IO ()
runDataSource opts ds = do
  inThread  <- async $ (atomically $ readTVar (opts^.mockStatus) >>= \r ->
                         unless (r == Waiting) retry)
               >> runInput
  outThread <- async $ runOutput
  mapM_ wait [inThread,outThread]
    where
      runInput = do
        tTarget <- spoolToTime
        forM_ (ds^.dsProducers) $ \p ->
          P.runEffect $ P.for (p >-> PP.takeWhile (\a -> (ds^.dsGetTS $ a) <= tTarget))
          (\a -> lift . atomically . modifyTVar (ds^.dsWaiting) $
                 M.insert (ds^.dsGetTS $ a) a)

      runOutput = P.runEffect $
                  P.for (outProducer >-> relativeTimeCat (ds^.dsGetTS))
                  (lift . atomically . writeTQueue (ds^.dsPub.chan))
      outProducer = forever $ do
        v <- lift $ atomically $ do
          m <- readTVar $ ds^.dsWaiting
          when (M.null m) retry
          return $ snd . M.elemAt 0 $ m
        P.yield v

      spoolToTime :: IO ExperimentTime
      spoolToTime = do
        f <- atomically $ readTVar (opts^.mockWallTimeFun)
        t <- getCurrentTime
        return $ f t + opts^.readAheadSec
      
            
  

--TODO Make mockData work on general tracks
track :: Track
track = circularTrack (0,0) 0.57 0.5 0.25 0.15



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

