{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE BangPatterns      #-}

module Data.Ephys.OldMWL.ParseSpike where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put           (putWord16le,putWord32le)
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.List                 as List
import           Data.Maybe                (listToMaybe)
--import           Data.Packed.Matrix        ((><))
--import qualified Data.Packed.Matrix        as HMatrix
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import qualified Data.Vector.Generic       as VG
import qualified Data.Vector.Unboxed       as U
import qualified Data.Vector.Binary
import           GHC.Int
import           Pipes
import qualified Pipes.Binary              as PBinary
import           Pipes.Binary              (decoded, decodeGetL,decodeGet)
import qualified Pipes.ByteString          as PBS
import           Pipes.Parse               (runStateT)
import qualified Pipes.Prelude             as PP
------------------------------------------------------------------------------
import           Data.Ephys.EphysDefs
import           Data.Ephys.OldMWL.FileInfo
import           Data.Ephys.OldMWL.Parse
import           Data.Ephys.OldMWL.Header
import qualified Data.Ephys.Spike          as Arte

------------------------------------------------------------------------------
data MWLSpike = MWLSpike { mwlSpikeTime      :: Double
                         , mwlSpikeWaveforms :: [U.Vector Double] -- Double is
                                                                  -- MWL units
                                                                  -- though
                         } deriving (Eq, Show)


------------------------------------------------------------------------------
toChans :: SpikeFileInfo -> U.Vector Voltage -> [U.Vector Voltage]
toChans info@SpikeFileInfo{..} vIn = map (toChan info vIn) [0..nChans - 1]

toChan :: SpikeFileInfo -> U.Vector Voltage -> Int -> U.Vector Voltage
toChan SpikeFileInfo{..} vIn chanInd =
  U.map (vIn U.!) $
  U.iterateN nSampsPerChan (+nChans) chanInd

------------------------------------------------------------------------------
data SpikeFileInfo = SpikeFileInfo {
    totalSamplesPerSpike :: !Int
  , nChans               :: !Int
  , nSampsPerChan        :: !Int
  , chanGains            :: !ChanGains
  }

type ChanGains = V.Vector Double

------------------------------------------------------------------------------
parseSpike :: SpikeFileInfo -> Get MWLSpike
parseSpike info@SpikeFileInfo{..} = do
  ts <- getWord32le :: Get Word32
  let tsDouble = decodeTime ts :: Double
      fI       = fromIntegral
  vs <- VG.replicateM totalSamplesPerSpike getWord16le :: Get (U.Vector Word16)
  let voltageArray = toChans info $
                     U.map (fI . word16ToInt16) vs :: [U.Vector Voltage]
  return $ MWLSpike tsDouble voltageArray
{-# INLINE parseSpike #-}


------------------------------------------------------------------------------
produceMWLSpikes :: SpikeFileInfo -> BSL.ByteString -> Producer MWLSpike IO ()
produceMWLSpikes si b =
  let myGet = parseSpike si :: Get MWLSpike
      bytes = PBS.fromLazy . dropHeaderInFirstChunk $ b
  in dropResult (getMany myGet bytes)
{-# INLINE produceMWLSpikes #-}


------------------------------------------------------------------------------
fileInfoToSpikeInfo :: FileInfo -> Either String SpikeFileInfo
fileInfoToSpikeInfo fi@FileInfo{..} =
  case listToMaybe $ filter (\(n,_,_) -> n == "waveform") hRecordDescr of
    Nothing                -> Left "Bad FileInfo" -- TODO elaborate
    Just (_,_,spikeNSamps) -> Right $ SpikeFileInfo
                                     (fromIntegral spikeNSamps) 4
                                     (fromIntegral spikeNSamps `div` 4)
                                     (V.fromList $ fileGains fi)


------------------------------------------------------------------------------
produceMWLSpikesFromFile :: FilePath -> Producer MWLSpike IO ()
produceMWLSpikesFromFile fn = do
  fi'     <- liftIO $ getFileInfo fn
  r       <- liftIO $ loadRawMWL fn
  let si' =  join $ fileInfoToSpikeInfo <$> fi'
  case (r,si') of
    (Right (_,dataBits), Right si) ->
      dropResult $ produceMWLSpikes si dataBits
    _ -> error $ "Couldn't open filename, or bad header: " ++ fn

produceTrodeSpikes :: Int -> FileInfo -> BSL.ByteString
                   -> Producer Arte.TrodeSpike IO ()
produceTrodeSpikes tName fi b =
  case fileInfoToSpikeInfo fi of
    Left e   -> error $ "Couldn't get spike info from file info: " ++ e
    Right si -> 
      produceMWLSpikes si b >-> PP.map (mwlToArteSpike fi tName)

-- TODO Int to TrodeName
produceTrodeSpikesFromFile :: FilePath -> Int -> Producer Arte.TrodeSpike IO ()
produceTrodeSpikesFromFile fn trodeName = do
  fi' <- liftIO . getFileInfo $ fn
  r'  <- liftIO . loadRawMWL  $ fn
  case (fi',r') of
    (Right fi, Right (_,dataBytes)) ->
     dropResult $ produceTrodeSpikes trodeName fi dataBytes
    (Left e1, Left e2) ->
      error $ fn ++ ": Bad fileinfo and file: " ++ e1 ++ " " ++ e2
    (Left e1,_) -> error $ fn ++ ": Bad fileinfo: " ++ e1
    (_,Left e2) -> error $ fn ++ ": Bad file: " ++ e2   

mwlToArteSpike :: FileInfo -> Int -> MWLSpike -> Arte.TrodeSpike
mwlToArteSpike fi tName s = Arte.TrodeSpike tName tOpts tTime tWaveforms
  where tTime      = mwlSpikeTime s
        gains      = V.fromList $ fileGains fi
        tWaveforms = V.zipWith
                     (\g -> U.map (mwlUnitsToVoltage g))
                     gains
                     (V.fromList $ mwlSpikeWaveforms s)
        tOpts = 1001 -- TODO: Get trodeopts
{-# INLINE mwlToArteSpike #-}

-- "path/to/0224.tt" -> "24"
-- TODO : Fix.  Only drop 5 when extention has 2 letters.
mwlTrodeNameFromPath :: String -> T.Text
mwlTrodeNameFromPath = T.pack . reverse . take 2 . drop 5 . reverse  


catSpike :: (Monad m) => Pipe Arte.TrodeSpike Arte.TrodeSpike m r
catSpike = forever $ do
  s <- await
  yield s

catSpike' :: Pipe MWLSpike MWLSpike IO r
catSpike' = forever $ do
  s <- await
  lift $ putStrLn "catSpike'"
  yield s



-- MWL units go as -2^13 -> (2^13-1)  => -10V -> 10V
mwlUnitsToVoltage :: Double -> Double -> Double
mwlUnitsToVoltage gain inV = inV * cVG/bMWL + (zMWL / bMWL)
  where bMWL= 2^(13::Int) - 1
        zMWL = -1/2^(15::Int)
        cVG  = 10 / gain

voltageToMwlUnits :: Double -> Double -> Double
voltageToMwlUnits gain inV = inV / (mwlUnitsToVoltage gain 1.0)
    

chunkToLength :: [a] -> Int -> [[a]]
chunkToLength xs n = aux [] xs
  where aux acc []  = Prelude.reverse acc
        aux acc xs' = aux (Prelude.take n xs' : acc) (Prelude.drop n xs')


------------------------------------------------------------------------------
fileGains :: FileInfo -> [Double]
fileGains FileInfo{..} =
  let gains' =
        map (\(ChanDescr ampGain _ _ _ _) -> ampGain) hChanDescrs in
  case hProbe of
    0 -> take 4 gains'
    1 -> take 4 . drop 4 $ gains'
    n -> error $ "Can't have probe " ++ show n
{-# INLINE fileGains #-}

------------------------------------------------------------------------------
writeSpike :: FileInfo -> MWLSpike -> Put
writeSpike fi (MWLSpike tSpike waveforms) = do
  putWord32le $ encodeTime tSpike
  let vs = List.concat . List.transpose . map U.toList $ waveforms
  forM_ vs $ 
    putWord16le . int16toWord16 . floor 
{-# INLINE writeSpike #-}
