{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------
-- collectDecodingBenchData
--
-- Output: several files with lists of positions or spikes
--         for benchmarking decoding algorithms
-------------------------------------------------------------------

module Main where

import qualified Data.ByteString.Lazy as BSL
import           Options.Applicative
import           Pipes
import qualified Pipes.Prelude        as PP
import           System.Environment

import           Data.Ephys.Position
import           Data.Ephys.Spike
import           Data.Ephys.OldMWL.ParsePFile
import           Data.Ephys.OldMWL.FileInfo
import           Data.Ephys.OldMWL.ParseSpike

data CollectionOpts = CollectionOpts {
    ttPath        :: FilePath
  , pPath         :: FilePath
  , trainingRange :: [Double]
  , testingRange  :: [Double]
  , outputDir     :: FilePath
  } deriving (Show)

--------------------------------------------------------------------------------
collectionOpts :: Parser CollectionOpts
collectionOpts =
  CollectionOpts
  <$> strOption (long "ttfile" <> short 't' <> metavar "TTFILE"
                 <> help "Path to a tt file"
                 <> value "~/Data/caillou/112812clip2/1328/1328.tt")
  <*> strOption (long "pfile" <> short 'p' <> metavar "PFILE"
                 <> help "Path to a p file"
                 <> value "~/Data/caillou/112812clip2/l28.p")
  <*> option auto (long "trainingrange" <> short 'r'
                   <> help "Training data time range")
  <*> option auto (long "testingrange" <> short 'e'
                   <> help "Testing data time range")
  <*> strOption (long "outputpath" <> short 'o' <> metavar "DIRECTORY"
                 <> help "Base path for output files")

pos0 :: Position
pos0 = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0
       ConfSure sZ sZ (-100 :: Double) (Location 0 0 0)
  where sZ = take 5 (repeat 0)

-- caillou/112812clip2 MWL-to-SIUnits TODO make general
posCenter :: (Double,Double)
posCenter = (166,140)

posRadius, posHeight :: Double
posRadius = 156.6
posHeight = 0.5

collectData :: CollectionOpts -> IO ()
collectData CollectionOpts{..} = do

  -- Get Position data during training & testing interval
  let pProducer   = produceMWLPosFromFile pPath
      runningP    = runningPosition
                    posCenter posRadius posHeight pos0
      pPred rng p = _posTime p >= (rng !! 0) &&
                    _posTime p <  (rng !! 1)

  posTrain <- PP.toListM (pProducer >-> runningP
                          >-> PP.filter (pPred trainingRange))
  posTest  <- PP.toListM (pProducer >-> runningP
                          >-> PP.filter (pPred testingRange))

  -- Get TrodeSpike data during trainging & testing interval
  let sPred rng s = spikeTime s >= (rng !! 0) &&
                    spikeTime s <  (rng !! 1)

  spkTrain <- PP.toListM (produceTrodeSpikesFromFile ttPath 0
                          >-> PP.filter (sPred trainingRange))
  spkTest  <- PP.toListM (produceTrodeSpikesFromFile ttPath 0
                          >-> PP.filter (sPred testingRange))

  print spkTest

main :: IO ()
main = do
  opt <- execParser opts
  pPath' <- handleHomeWildcard (pPath opt)
  ttPath' <- handleHomeWildcard (ttPath opt)
  collectData (opt {pPath = pPath', ttPath = ttPath'})
       where opts =
               info (helper <*> collectionOpts)
               ( fullDesc
                 <> progDesc "Produce some training and test data"
                 <> header "collectBenchData - arte-decoder benchmarking data")

handleHomeWildcard :: FilePath -> IO FilePath
handleHomeWildcard fPath
  | '~' `elem` fPath && head fPath == '~' =
    (++ tail fPath) <$> getEnv "HOME"
  | otherwise = return fPath
