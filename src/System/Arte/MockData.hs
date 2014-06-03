{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, TemplateHaskell, TupleSections, OverloadedStrings #-}

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
import Data.Configurator hiding (prefix)
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
import Data.Text hiding (reverse, dropWhile, drop)
import qualified Data.Text as T hiding (reverse, dropWhile, drop)
import qualified Data.Map as M
import qualified Data.Serialize as S
import Control.Lens

data MockStatus = Seeking | Waiting | Streaming
                deriving (Eq, Show, Read)

data DataFormat = JSON | Binary
                deriving (Eq, Show, Read)

data InputFileType = MwlEEG | MwlTT | MwlP
                   deriving (Eq, Show, Read)

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
getSourceOpts conf = catMaybes <$> sources
  where sources =
          forM [(0::Int)..9] $ \i -> do
           let prefix = T.pack . (("source" ++ show i ++ ".") ++)
           dp' <- Conf.lookup conf $ prefix "dataPort"
           files <- Conf.lookup conf $ prefix "files"
           fType <- Conf.lookup conf $ prefix "fileType"
           fmt   <- Conf.lookup conf $ prefix "outFormat"
           bp    <- Conf.lookup conf "dataBasePath"
           case (dp', files, fType, fmt,bp) of
             (Just p, Just fl, Just fT, Just fM, Just basePath) -> do
               let node = Node "source"
                          (Host (T.unpack $ prefix "host") "localhost")
                          (fromInt . read $ p)
               expPaths <- processPath (basePath ++ fl)
               return . Just $ DataSourceOpts node (read fM) expPaths (read fT)
             _ -> return Nothing

getMockOpts :: FilePath -> IO (Either String MockOpts)
getMockOpts f = do
  config <- Conf.load [Conf.Required f]
  runEitherT $ do
    seekWait  <- noteT "Missing wait option" . MaybeT $
                 Conf.lookup config "waitAfterSeeking"
    lookAhead <- noteT "Missing look ahead" . MaybeT $
                 Data.Configurator.lookup config "readAheadSeconds"
--    basePath  <- noteT "Missing base directory" . MaybeT $
--                 Data.Configurator.lookup config "dataBasePath"
    cmdPort   <- noteT "Missing command port" . MaybeT $
                 Data.Configurator.lookup config "commandPort"
    seekTime  <- noteT "Missing default seek time" . MaybeT $
                 Data.Configurator.lookup config "initialSeekTime"
    return $ MockOpts
      (Node "commandNode" (Host "MockData" "localhost") 
       (fromInt . read $ cmdPort))
      (read lookAhead) seekWait (read seekTime)

fromInt :: Integral a => Int -> a
fromInt = fromIntegral

main :: IO ()
main = do
  undefined

class Timestamp a where
  getTS :: a -> ExperimentTime

instance Timestamp TrodeSpike where
  getTS = spikeTime

instance Timestamp Position where
  getTS = _posTime

data AnyDataSource = DSSpikes (DataSource TrodeSpike)
                   | DSPos    (DataSource Position)

------------------------------------------------------------------------------
mkSource :: Conf.Config -> MockOpts -> DataSourceOpts -> IO (AnyDataSource)
mkSource cfg mock source = do
  q <- atomically . newTVar $ M.empty
  s <- atomically . newTVar $ Seeking
  p <- DataPublisher <$> (atomically $ newTQueue) <*> newTVarIO []
  case (source^.dsFileType) of
    MwlTT -> do
      prods <- forM (source^.dsFilePaths) $ \fn -> do
        let tName = read . reverse . dropWhile (/= '.') . drop 3 . reverse $ fn :: TrodeName
        (getMWLSpikeProducer fn tName)
      return . DSSpikes $ DataSource (catMaybes prods) q p
    MwlP  -> do
      prod <- getMWLPProducer cfg (

{-
          (posTime, getMWLPProducer)
      x = fileSpecifics :: (a -> ExperimentTime, Conf.Config -> FilePath -> IO(Maybe(P.Producer a IO ())))
  let node = source ^. dsNode
      format = Main.Binary
      tTarg = mock ^. mockFirstSeekTime
  case fileSpecifics of
    (timeFun, Just getProducer) -> do
      prods' <- forM (source^.dsFilePaths) $ \fn -> do
        producer <- getProducer fn
        (producer >-> Prelude.dropWhile (\a -> timeFun a < tTarg))
      return $ DataSource (seq prods' prods') q timeFun p node format
-}

getMWLSpikeProducer :: FilePath -> TrodeName ->
                       IO (Maybe (P.Producer TrodeSpike IO ()))
getMWLSpikeProducer fn tName = do
  f  <- BSL.readFile fn
  fi' <- getFileInfo fn
  case fi' of
    Right fi -> 
      return $ Just (dropResult (produceTrodeSpikes tName fi f ))
    Left _ -> return Nothing

getMWLPProducer :: Conf.Config -> FilePath ->
                   IO (Maybe (P.Producer Position IO ()))
getMWLPProducer cfg fp = do
  hints' <- runMaybeT $ do
    pxX <- MaybeT $ Conf.lookup cfg "mwlPHints.originXPixel"
    pxY <- MaybeT $ Conf.lookup cfg "mwlPHints.originXPixel"
    pS  <- MaybeT $ Conf.lookup cfg "mwlPHints.pxPerMeter"
    pH  <- MaybeT $ Conf.lookup cfg "mwlPHints.trackHeight"
    return $ PosMWLShim pxX pxY pS pH
  case hints' of
    Nothing -> return Nothing
    Just hints -> return . Just $ producePosition hints fp

------------------------------------------------------------------------------
runDataSource :: Timestamp a => MockOpts -> MockState -> DataSourceOpts ->
                 DataSource a-> IO ()
runDataSource opts mockS dsOpt dsState = do
  inThread  <- async $ (atomically $ readTVar (mockS^.mockStatus) >>= \r ->
                         unless (r == Waiting) retry)
               >> runInput
  outThread <- async $ runOutput
  mapM_ wait [inThread,outThread]
    where
      runInput = do
        tTarget <- spoolToTime
        forM_ (dsState^.dsProducers) $ \p ->
          P.runEffect $ P.for (p >-> PP.takeWhile (\a -> (getTS a) <= tTarget))
          (\a -> lift . atomically . modifyTVar (dsState^.dsWaiting) $
                 M.insert (getTS a) a)

      runOutput = P.runEffect $
                  P.for (outProducer >-> relativeTimeCat getTS)
                  (lift . atomically . writeTQueue (dsState^.dsPub.chan))
      outProducer = forever $ do
        v <- lift $ atomically $ do
          m <- readTVar $ dsState^.dsWaiting
          when (M.null m) retry
          return $ snd . M.elemAt 0 $ m
        P.yield v

      spoolToTime :: IO ExperimentTime
      spoolToTime = do
        t <- getCurrentTime
        f <- readTVarIO $ mockS^.mockWallTimeFun
        return $ f t + opts^.mockReadAheadTime
      
            
  

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

