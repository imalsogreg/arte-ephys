{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module System.Arte.MockData.FakeRat where

import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Data.Aeson           as A
import           Data.Bool            (bool)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.List
import           Data.Maybe
import qualified Data.Serialize       as S
import           Data.Time
import           GHC.Word
import           Network
import           Network.Socket
import qualified Network.Socket.ByteString as BS
import           Pipes
import           Pipes.RealTime
import           System.Random

import Data.Ephys.Position
import Data.Ephys.TrackPosition
import Types

data State = State {
    sPos  :: Location
  , sGoal :: Location
} deriving (Eq, Show)

locSum :: Location -> Location -> Location
locSum (Location a b c) (Location x y z) = Location (a+x) (b+y) (c+z)

locProd :: Double -> Location -> Location
locProd c (Location x y z) = Location (x*c) (y*c) (z*c)

locUnit :: Location -> Location
locUnit (Location x y z) = let c = sqrt $ x^2 + y^2 + z^2
                           in Location (x/c) (y/c) (z/c)

locDiff' :: Location -> Location -> Location
locDiff' l l' = let (x,y,z) = locDiff l l' in Location x y z

locMag :: Location -> Double
locMag (Location x y z) = sqrt $ x^2 + y^2 + z^2

goal0 = (_location p0)
goal n = goals !! (n `mod` 8)

goals :: [Location]
goals = map (\t -> Location (1.5 * cos t) (1.5 * sin t) 0.5)
        [pi/4, pi/2 .. 2*pi]

stepPos' :: State -> IO State
stepPos' (State p g) = do
  speed <- randomRIO (0.01,0.05)
  rndX  <- randomRIO (0.01,0.05)
  rndY  <- randomRIO (0.01,0.05)
  n     <- randomRIO (0,7)
  let goalVec  = locSum (locDiff' p g) (Location rndX rndY 0)
      runVec   = locProd speed (locUnit goalVec)
      nextPos  = locSum (p) runVec
      nextGoal = bool g toglGoal (locMag goalVec < 0.01)
      toglGoal = bool goal0 (goal n) (g == goal0)
      s'       = State nextPos nextGoal
  print s'
  return s'


p0 :: Position
p0 = Position 0 (Location 0 0 0) (Angle 0 0 0) 0 0 ConfSure
        (replicate 5 0) (replicate 5 0) (0 - 0.03) (Location 0 0 0)


streamFakePos :: DataSourceOpts -> IO ()
streamFakePos DataSourceOpts{..} = withSocketsDo $ do
  sock <- socket AF_INET Datagram defaultProtocol
  t0 <- getCurrentTime
  let saddr = SockAddrInet (fromIntegral myPort) iNADDR_ANY
      enc   = case outputFormat of
                ArteBinary -> S.encode
                ArteJSON   -> BSL.toStrict . A.encode
  destAddr <- SockAddrInet
                  (fromIntegral (destPort :: Word32))
                  <$> inet_addr ipAddy
  let go p s  = do
                 s' <- stepPos' s
                 now <- getCurrentTime
                 let t' = realToFrac (diffUTCTime now t0) +
                          realToFrac expStartTime
                     p' = stepPos p t' (sPos s') (Angle 0 0 0) ConfSure
                 BS.sendAllTo sock (enc (p')) destAddr
                 threadDelay 30000
                 go p' s'
  destAddr <- SockAddrInet
              (fromIntegral (destPort :: Word32))
              <$> inet_addr ipAddy
  bindSocket sock saddr
  go p0 (State (_location p0) (goal 1))
