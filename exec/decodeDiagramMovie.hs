module Main where

import           Control.Applicative
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector           as V
import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude
import qualified System.IO.Streams as S
import           System.Environment
import           Codec.FFmpeg
import           Codec.Picture
import           Codec.Picture.Types
import           Data.Ephys.DiagramsPictures
import           Data.Ephys.TrackPosition

------------------------------------------------------------------------------
main :: IO ()
main = do
  [fnIn,fnOut,nDrop,nTake] <- getArgs
  initFFmpeg
  putStrLn "Make Output stream"
  outVid <- S.makeOutputStream =<<
            (imageWriter (defaultParams sizeX sizeY) fnOut :: IO (Maybe (Image PixelRGB8) -> IO ()))
  putStrLn "Make InputStream"
  ls <- S.fromLazyByteString =<< BSL.readFile fnIn
  ds <- S.take (read nTake) =<<
        S.drop (read nDrop) =<<
        S.map (fieldToPic . lineToField) ls
  putStrLn "Run Connection"
  S.connect ds outVid


sizeX, sizeY :: Num a => a
sizeX = 64
sizeY = 64

trackFromFile :: Track
trackFromFile = circularTrack (0,0) 0.57 0.5 0.25 0.2

lineToField :: BS.ByteString -> LabeledField Double
lineToField l =
  let nums = map (read . BS.unpack) .
             drop 1 . map (BS.filter (/= ' ')) .
             BS.split ',' $ l :: [Double]
  in  labelField trackFromFile (V.fromList nums)

fieldToPic :: LabeledField Double -> Image PixelRGB8
fieldToPic f = pixelMap dropTransparency $ renderDia Rasterific opts (fieldDiagram f)
  where opts = RasterificOptions Absolute
