{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Arte.Decode.Types where

------------------------------------------------------------------------------
import           Control.Concurrent.STM.TVar
import           Control.Lens
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Serialize
import           Data.Time
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import           Options.Applicative
------------------------------------------------------------------------------
import           Data.Ephys.EphysDefs
import           Data.Ephys.TrackPosition
import           Data.Ephys.PlaceCell
import           Data.Ephys.Position
import           Data.Map.KDMap
import           System.Arte.Decode.Histogram

-- TODO: This module would be a lot clearer with some organization

------------------------------------------------------------------------------
data DecoderState = DecoderState
                    { _pos           :: TVar Position
                    , _trackPos      :: TVar (Field)
                    , _occupancy     :: TVar (Field)
                    , _maybeunused   :: TVar (Field)
                    , _trodes        :: Trodes
                    , _decodedPos    :: TVar (Field)
                    , _trodeDrawOpt  :: TrodeDrawOptions
                    , _trodeInd      :: Int
                    , _clustInd      :: Int
                    , _drawKDESample :: Bool
                    , _toExpTime     :: UTCTime -> Double
                    , _samplePoint   :: Maybe ClusterlessPoint

                    , _encodeProf    :: Histogram Double
                    , _decodeProf    :: Histogram Double
                    }

------------------------------------------------------------------------------
-- Placeholder.  Will be more like: KdTree (Vector Voltage) (Field Double)
type SpikeHistory = Int

nullHistory :: SpikeHistory
nullHistory = 0

runningThresholdSpeed :: Double
runningThresholdSpeed = 0.30;

data DecodablePlaceCell = DecodablePlaceCell { _dpCell     :: !PlaceCell
                                             , _dpCellTauN :: !Int
                                             } deriving (Eq, Show)

data PlaceCellTrode = PlaceCellTrode {
    _dUnits :: Map.Map PlaceCellName (TVar DecodablePlaceCell)
  , _pcTrodeHistory :: !SpikeHistory
  } deriving (Eq)

type NotClust = KDMap ClusterlessPoint MostRecentTime

data ClusterlessTrode = ClusterlessTrode
                        { _dtNotClust :: NotClust
                        , _dtTauN     :: [(ClusterlessPoint,MostRecentTime,Bool)]
                        } deriving (Eq, Show)


data Trodes = Clusterless (Map.Map TrodeName (TVar ClusterlessTrode))
            | Clustered   (Map.Map TrodeName PlaceCellTrode)



------------------------------------------------------------------------------
type TrodeCollection a = Map.Map TrodeName (Map.Map PlaceCellName a)


------------------------------------------------------------------------------
-- Field is in the Key, not the Payload, because it needs to be
-- weighted-summed, not just summed, and payload append doesn't
-- consider the weights (althought it should - so, TODO refactor KDMap
-- add a KDValue class with weightedAppend method)
data ClusterlessPoint = ClusterlessPoint {
    _pAmplitude  :: U.Vector Voltage
  , _pWeight     :: !Double
  , _pField      :: Field
  } deriving (Eq, Show)



------------------------------------------------------------------------------
instance Monoid ClusterlessPoint where
  mempty = ClusterlessPoint (U.fromList []) 0 (V.empty :: Field)
  a `mappend` b = ClusterlessPoint
                  (U.zipWith weightedSum (_pAmplitude a) (_pAmplitude b))
                  (_pWeight a + _pWeight b)
                  (V.zipWith weightedSum (_pField a) (_pField b))
    where
      weightedSum s t  = let wA = _pWeight a
                             wB = _pWeight b
                             wR =1/ (wA + wB)
                         in  (s*wA + t*wB) * wR

instance KDKey ClusterlessPoint where
  pointD p i  = _pAmplitude p U.! (fromIntegral i)
  pointSize p = fromIntegral . U.length $ _pAmplitude p
  pointW p    = realToFrac $ _pWeight p
  dSucc p d   = succ d `mod` pointSize p
  dPred p d   = pred d `mod` pointSize p


------------------------------------------------------------------------------
newtype MostRecentTime = MostRecentTime { unMostRecentTime :: Double}
                         deriving (Eq,Ord,Show,Num,Real)


instance Monoid MostRecentTime where
  mempty = MostRecentTime (-1/0)
  a `mappend` b = max a b


------------------------------------------------------------------------------
newtype XChan = XChan Int deriving (Eq)
newtype YChan = YChan Int deriving (Eq)


------------------------------------------------------------------------------
data ClessDraw = ClessDraw XChan YChan
  deriving (Eq)


------------------------------------------------------------------------------
data TrodeDrawOption =
    DrawPlaceCell   PlaceCellName (TVar DecodablePlaceCell)
  | DrawClusterless TrodeName     (TVar ClusterlessTrode) ClessDraw
  | DrawOccupancy
  | DrawDecoding
  | DrawError String
  deriving (Eq)


------------------------------------------------------------------------------
instance Show TrodeDrawOption where
  show (DrawPlaceCell n _)     = "DrawPlaceCell " ++ show n
  show (DrawClusterless n _ _) = "DrawClusterless " ++ show n
  show DrawOccupancy           = "DrawOccupancy"
  show DrawDecoding            = "DrawDecoding"
  show (DrawError s)           = "DrawError " ++ s

type TrodeDrawOptions = [[TrodeDrawOption]]

$(makeLenses ''PlaceCellTrode)
$(makeLenses ''DecodablePlaceCell)
$(makeLenses ''ClusterlessPoint)
$(makeLenses ''Trodes)
$(makePrisms ''Trodes)
$(makeLenses ''ClusterlessTrode)


------------------------------------------------------------------------------
data DecoderArgs = DecoderArgs {ttDir               :: FilePath
                               ,startExperimentTime :: Double
                               ,doLogging           :: Bool
                               ,clusterless         :: Bool
                               ,decodingInterval    :: Double
                               ,trodeName           :: String
                               }
                 deriving (Show)

decoderArgs :: Parser DecoderArgs --update to include trodeName
decoderArgs = DecoderArgs
              <$> strOption
              ( long "ttDir"
              <> help ("Path to a directory w/ clusder bounds and a " ++
                      ".tt file for the channel gains (??)"))
              <*> option auto
              ( long "startExperimentTime"
              <> value 0
              <> help "Start time when spooling from disk")
              <*> flag False True
              ( long "doLogging"
              <> help "Commit timing/decoding data to log files")
              <*> flag False True
              ( long "clusterless"
              <> help "Perform clusterless decoding")
              <*> option auto
              ( long "decodingInterval"
              <> help "Time interval for emitting position estimates")
              ( long "trodeName"
              <> help "Name of the tetrode we are recording from")
              <*> strOption
              ( long "estimateIP"
              <> help "IP address to send decoding estimates to")
              <*> option auto
              ( long "estimatePort"
              <> help "Port to send decoding estimates to")

decoderOpts = info (helper <*> decoderArgs)
       (fullDesc
       <> progDesc "Decode position from spikes"
       <> header "arte-decoder")

data Packet = Packet {
    packetEstimate :: Field
  , packetTime     :: SOME KIND OF TIME
  , packetTrode    :: TrodeName
} deriving (Generic)

instance Serialize Packet where
