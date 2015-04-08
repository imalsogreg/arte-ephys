{-# LANGUAGE TemplateHaskell #-}

module Data.Ephys.OldMWL.ParsePFile where

import Data.Ephys.Position
import Data.Ephys.OldMWL.FileInfo
import Data.Ephys.OldMWL.Parse (decodeTime, encodeTime, dropResult, getMany)
import Data.Ephys.OldMWL.Header

import Control.Lens
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Pipes.ByteString as PBS
import qualified Pipes.Binary as PBinary
import Pipes
import qualified Pipes.Prelude as PP
import qualified Data.Binary as Binary
import Data.Binary.Put
import Data.Binary.Get (getWord32le, getWord16le, Get(..))

data MWLPos = MWLPos { _mwlPosTime  :: !Double
                     , _mwlPxf      :: !Int
                     , _mwlPyf      :: !Int
                     , _mwlPxb      :: !Int
                     , _mwlPyb      :: !Int
                     } deriving (Eq, Show)

$(makeLenses ''MWLPos)

--produceMWLPos :: BSL.ByteString ->
--                 Producer MWLPos IO (Either (PBinary.DecodingError,
--                                             Producer BS.ByteString IO ()) ())
produceMWLPos :: BSL.ByteString -> Producer MWLPos IO ()
produceMWLPos f =
  let bytes = PBS.fromLazy . dropHeaderInFirstChunk $ f
  in dropResult $ getMany Binary.get bytes

data PosMWLShim = PosMWLShim { 
    shimOriginXPixel :: !Int
  , shimOriginYPixel :: !Int
  , shimPxPerMeter   :: !Double
  , shimTrackHeight  :: !Double
  }
                  
producePosition :: PosMWLShim -> FilePath -> Producer Position IO ()
producePosition sh fp = produceMWLPosFromFile fp >->
                        runningPosition (x0,y0) s h nullPosition
  where (x0,y0) = (fromIntegral $ shimOriginXPixel sh,
                   fromIntegral $ shimOriginYPixel sh)
        s       = shimPxPerMeter  sh 
        h       = shimTrackHeight sh

produceMWLPosFromFile :: FilePath -> Producer MWLPos IO ()
produceMWLPosFromFile fn = do
  r <- liftIO $ loadRawMWL fn
  f <- liftIO $ BSL.readFile fn
  case r of
    Right (_,_) -> dropResult $ produceMWLPos f
    Left e      -> error $ "Couldn't open mwl p file: " ++
                   fn ++ " error: " ++ e

parsePRecord :: Binary.Get MWLPos
parsePRecord = do
    recTs    <- getWord32le
    recXf    <- getWord16le
    recYf    <- getWord16le
    recXb    <- getWord16le
    recYb    <- getWord16le
    return $ MWLPos (decodeTime recTs)
      (fI recXf) (fI recYf) (fI recXb) (fI recYb)

instance Binary.Binary MWLPos where
  get = parsePRecord
  put (MWLPos t xf yf xb yb) = do
    putWord32le $ encodeTime t
    putWord16le $ fromIntegral xf
    putWord16le $ fromIntegral yf
    putWord16le $ fromIntegral xb
    putWord16le $ fromIntegral yb

mwlToArtePos :: (Double,Double)
             -> Double
             -> Double
             -> MWLPos
             -> Position
             -> Position
mwlToArtePos (pX0,pY0) pixelsPerMeter height m p =
  let s = 1/pixelsPerMeter
      pXToArte = (*s) . subtract pX0
      pYToArte = (*s) . subtract pY0
      fX = fI $ m^.mwlPxf :: Double
      fY = fI $ m^.mwlPyf :: Double
      bX = fI $ m^.mwlPxb :: Double
      bY = fI $ m^.mwlPyb :: Double
      xArte = pXToArte $ avg2 fX bX
      yArte = pYToArte $ avg2 fY bY
      loc = Location xArte yArte height
      angleArte = Angle (atan2 (fY - bY) (fX - bX)) 0 0
      conf = if all (> 0) [fX,bX,fY,bY] then ConfSure else ConfUnsure
  in
  stepPos p (m^.mwlPosTime) loc angleArte conf

runningPosition :: (Monad m) => 
                  (Double,Double) ->
                  Double ->
                  Double -> 
                  Position -> Pipe MWLPos Position m r
runningPosition (pX0, pY0) pixPerMeter height = 
  loop
    where 
      loop p0 = do
        mwlP <- await
        let p = mwlToArtePos (pX0,pY0) pixPerMeter height mwlP p0 :: Position
        yield p
        loop p

avg2 :: Double -> Double -> Double
avg2 a b = (a+b)/2

fI :: (Num a, Integral b) => b -> a
fI = fromIntegral

catPos :: Monad m => Pipe MWLPos MWLPos m r
catPos = do
  p <- await
  yield p
  catPos
