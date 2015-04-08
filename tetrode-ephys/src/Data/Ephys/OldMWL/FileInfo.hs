{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Data.Ephys.OldMWL.FileInfo where
       
--------------------------------------------------------------------------------
-- FileHeader.hs 
--
-- Parse enough of MWL file headers to extract data from them in arte format
-- (eg - need to get channel gains, possibly thresholds, record formatting)
--
-- MWL headers are a little messy because MWL files go through a couple
-- processing steps, with intermediate data saved in intermediate files,
-- and each file lists the headers of the files of the preceding steps
--
-- I'll mostly ignore this, and try to tailor the header-loading to a
-- couple of file types, selectively ignoring parts of the total
-- header that I know aren't needed for specific uses of the file.
-- (eg - header of a tt file will discard the 'original ad file' field.)
--
-- AD files and tt files have a different format for the 'fields' field.
-- I may not have dealt with this the right way for all types of files.
--------------------------------------------------------------------------------

import Control.Monad
import Control.Applicative ((<$>),(<*>))
import Data.Traversable (traverse)
import Text.ParserCombinators.Parsec
import Data.List (isSuffixOf)
import Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL

data FileType = Binary | Ascii deriving (Eq, Show, Read)

data RecordMode = Spike | Continuous | Tracker deriving (Eq, Show)

readRecordMode :: String -> RecordMode
readRecordMode "CONTINUOUS" = Continuous
readRecordMode "SPIKE"      = Spike
readRecordMode "TRACKER"    = Tracker

data ExtractionType = TetrodeWaveforms
                    | ContinuousData
                    | ExtendedDualDiodePosition
                    deriving (Eq, Show)

readExtractionType :: String -> ExtractionType
readExtractionType "tetrode waveforms" = TetrodeWaveforms
readExtractionType "continuous data"   = ContinuousData
readExtractionType "extended dual diode position" = ExtendedDualDiodePosition

type DatumName = String

type RecordDescr = (DatumName, DatumType, DatumRepeatCount)
data ChanDescr = ChanDescr {ampGain    :: Double
                           ,adGain     :: Double
                           ,filterCode :: Integer
                           ,threshold  :: Double
                           ,colorCode  :: Integer
                           }
                 deriving (Eq, Show)

data FileInfo = FileInfo { hProgram     :: String
                         , hVersion     :: String
                         , hArgv        :: [String]
                         , hDate        :: String
                         , hDir         :: String
                         , hHostname    :: String
                         , hArch        :: String
                         , hUser        :: String
                         , hFileType    :: FileType
                         , hExtractT    :: ExtractionType
                         , hProbe       :: Integer
                         , hRecordDescr :: [RecordDescr]
                         , hRate        :: Double
                         , hNTrodes     :: Integer
                         , hNChans      :: Integer
                         , hNTrodeChans :: Integer
                         , hRecMode     :: RecordMode
                         , hChanDescrs  :: [ChanDescr]
                         } deriving (Eq, Show)

dropHeaderInFirstChunk :: BSL.ByteString -> BSL.ByteString
dropHeaderInFirstChunk b = let headerEnd = BSC.pack "%%ENDHEADER\n"
                               firstChunk = head . BSL.toChunks $ b
                               (h,t) = BS.breakSubstring headerEnd firstChunk
                           in
                            if BS.null t
                            then b
                            else BSL.drop (fromIntegral (BS.length h + BS.length headerEnd)) b

parseFields :: CharParser () [RecordDescr]
parseFields = do
  fields <- many parseField
  return fields

parseField :: CharParser () RecordDescr
parseField = do
  fieldName <- many (noneOf ",")
  char ','
  datumCode <- many digit 
  char ','
  _  <- many digit  -- We ignore the size field from the file b/c it's fixed by the type
  char ','
  datumCount <- many digit
  many (char '\t')
  return (fieldName, datumTypeFromIntegral (read datumCode), read datumCount)

okSpikeFile :: FileInfo -> Bool
okSpikeFile FileInfo{..} = hRecMode == Spike
                           && hRecordDescr `hasField` "timestamp"
                           && hRecordDescr `hasField` "waveform"
                           && fieldIsType hRecordDescr "timestamp" DULong
                           && fieldIsType hRecordDescr "waveform"  DShort
                           -- waveform elem's type is DShort (16-bit)

okPosFile :: FileInfo -> Bool
okPosFile FileInfo{..} = hRecMode == Tracker
                         && hasField hRecordDescr "timestamp"
                         && hasField hRecordDescr "xfront"
                         && hasField hRecordDescr "yfront"
                         && hasField hRecordDescr "xback"
                         && hasField hRecordDescr "yback"

okPFile :: FileInfo -> Bool
okPFile FileInfo{..} = hRecMode == Tracker
                       && all (hRecordDescr `hasField`) ["timestamp","xfront","yfront","xback","yback"]
                       && all id (zipWith (fieldIsType hRecordDescr)
                               ["timestamp","xfront","yfront","xback","yback"]
                               [DShort, DShort, DShort, DShort])

hasField :: [RecordDescr] -> String -> Bool
hasField flds f = any (\(name,_,_) -> name == f) flds

fieldIsType :: [RecordDescr] -> String -> DatumType -> Bool
fieldIsType flds f t = Prelude.all (==True) [ (fn == f) <= (ft ==t) | (fn,ft,_) <- flds ]

-- This is fileinfo for SPIKE files and EEG files.  Should get a rename
getFileInfo :: FilePath -> IO (Either String FileInfo)
getFileInfo fn = do
  c   <- readFile fn :: IO String
  case paramStringsMap $ parse pFileHeader "MWL File Header" c of
    Left e -> return $ Left e
    Right pMap -> do
      let grab k = case Map.lookup k pMap of
            Just v  -> Right v
            Nothing -> Left $ "Error getting field " ++ show k
          nChans = fromIntegral . length $ Prelude.filter ("threshold" `isSuffixOf`) (Map.keys pMap)
          progNArg = read $ either (const "0") (id) $ grab "Argc" :: Integer  

      return (FileInfo    
                  <$> grab "Program"
                  <*> grab "Program Version"
                  <*> traverse grab ["Argv[" ++ show n ++ "]" | n <- [1..progNArg - 1]]
                  <*> grab "Date"
                  <*> grab "Directory"
                  <*> grab "Hostname"
                  <*> grab "Architecture"
                  <*> grab "User"
                  <*> read `liftM` grab "File type"
                  <*> readExtractionType `liftM` grab "Extraction type"
                  <*> read `liftM` grab "Probe"
                  <*> either (Left . show) (Right . id)    -- There must be
                  (parse parseFields "MWL Header Fields"   -- a better way
                   (either (const "") id $ grab "Fields")) -- to do this..
                  <*> read `liftM` grab "rate"
                  <*> read `liftM` grab "nelectrodes"
                  <*> read `liftM` grab "nchannels"
                  <*> read `liftM` grab "nelect_chan"
                  <*> readRecordMode `liftM` grab "mode"
                  <*> traverse (grabChannelDescr pMap) [0 .. nChans - 1])

grabChannelDescr :: Map String String -> Integer -> Either String ChanDescr
grabChannelDescr m n = let grab k = case Map.lookup k m of
                             Just v  -> Right v
                             Nothing -> Left $ "ChannelDescr error grabbing field " ++ k
                       in
  do
  ampGain'  <- grab (unwords ["channel", show n, "ampgain"])
  adGain'   <- grab (unwords ["channel", show n, "adgain"])
  filtCode <- grab (unwords ["channel", show n, "filter"])
  thresh   <- grab (unwords ["channel", show n, "threshold"])
  col      <- grab (unwords ["channel", show n, "color"])
  return $ ChanDescr (read ampGain') (read adGain') (read filtCode) (read thresh) (read col)

type DatumRepeatCount = Integer
data DatumType = DInvalid
               | DChar
               | DShort
               | DInt
               | DFloat
               | DDouble
               | DFunc
               | DFFunc
               | DULong
               | DUnknown
               deriving (Eq, Ord, Show)

datumTypeIntMap :: [(DatumType, Integer)]
datumTypeIntMap = [(DInvalid, 0),(DChar, 1),(DShort,2),(DInt,3)
                  ,(DFloat,4),(DDouble,5),(DFunc,6),(DFFunc,7)
                  ,(DULong,8),(DUnknown,-1)]
datumTypeToIntegral :: DatumType -> Integer
datumTypeToIntegral d = maybe (-1) id (Prelude.lookup d datumTypeIntMap)
datumTypeFromIntegral :: Integer -> DatumType
datumTypeFromIntegral i =
  maybe DUnknown id (Prelude.lookup i (Prelude.map(\(a,b)->(b,a)) datumTypeIntMap))


-- Parsec parsing of MWL Header
pFileHeader :: CharParser () [(String, Maybe String)]
pFileHeader = do
  string "%%BEGINHEADER\n"
  pairs <- many pHeaderLine
  string "%%ENDHEADER\n"
  return pairs
  
pHeaderLine :: CharParser () (String, Maybe String)
pHeaderLine = do
  entry <- try pPair
           <|> try pUnusedLine
           <|> try pMalformedPair
           <|> try pNonsenseLine
           <?> "unparsable line"
  return entry

pPair :: CharParser () (String, Maybe String)
pPair = do
  char '%'
  many (char '\t' <|> char ' ')
  name <- many pKeyChar
  value <- optionMaybe (pKVSep >> many pValChar)
  char '\n'
  return (stripTrailingSpaces name, value)

stripTrailingSpaces :: String -> String
stripTrailingSpaces s = reverse . dropWhile (== ' ') . reverse $ s

pNonsenseLine :: CharParser () (String, Maybe String)
pNonsenseLine = do
  char '%'
  many (noneOf ("\n%"))
  char '\n'
  return ("",Nothing)

pMalformedPair :: CharParser () (String, Maybe String)
pMalformedPair = do
  string "%"
  many (char '\t')
  spaces
  name <- many (noneOf "%:\n")
  char '\n'
  return (name,Nothing)

pUnusedLine :: CharParser () (String, Maybe String)
pUnusedLine = do
  string "%\n"
  return ("",Nothing)

pKVSep :: CharParser () [Char]
pKVSep = char ':' >> many (char ' ' <|> char '\t')

pKeyChar :: CharParser () Char
pKeyChar = noneOf ":\t\n%"

pValChar :: CharParser () Char
pValChar = noneOf "\n"

paramStringsMap :: Either ParseError [(String, Maybe String)] ->
                   Either String (Map.Map String String)
paramStringsMap (Left e) = Left $ show e
paramStringsMap (Right as) = Right $ Prelude.foldl insJustsWithoutReplacement Map.empty as
  where
    insJustsWithoutReplacement m (_, Nothing) = m
    insJustsWithoutReplacement m (k, Just v)  =
      case Map.lookup k m of
        Nothing  -> insert k v m
        (Just _) -> m

