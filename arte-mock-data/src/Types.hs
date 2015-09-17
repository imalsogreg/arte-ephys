module Types where

import GHC.Word
import Control.Applicative
import Options.Applicative

-- | Output format for pos/spikes coming from mock-data
--   (input format is determined by the file extension,
--    either .tt or .p)
data OutputFormat = ArteBinary
                  -- ^ tetrode-ephys Position/Spike Serialize encoding
                  | ArteJSON
                  -- ^ tetrode-ephys Position/Spike ToJSON encoding
                  | OldArteBinary
                  -- ^ old arte project's NetMessage binary encoding
                  | OatJSON
                  -- ^ Oat's json encoding
                  deriving (Eq, Show, Read)

data DataSourceOpts = DataSourceOpts
                      { outputFormat :: OutputFormat
                      , verbose      :: Bool
                      , fileName     :: FilePath
                      , ipAddy       :: String
                      , myPort       :: Word32
                      , destPort     :: Word32
                      , expStartTime :: Double }
                      deriving (Eq, Show)

dataSourceOpts :: Parser DataSourceOpts
dataSourceOpts = DataSourceOpts
                 <$> option auto
                 ( long "format"
                 <> help "Write to network in ArteBinary ArteJSON or OldArteBinary format")
                 <*> switch
                 ( long "verbose"
                 <> short 'v'
                 <> help "Print lots of output")
                 <*> strOption
                 ( long "file"
                 <> help "File to stream data from (must be .tt or .p)")
                 <*> strOption
                 (long "ip"
                  <> value "127.0.0.1"
                 <> help "Destination IP address")
                 <*> option auto
                 (long "localport"
                 <> help "Local sending port number")
                 <*> option auto
                 (long "destport"
                 <> help "Receiver's port at destination")
                 <*> option auto
                 (long "startTime"
                  <> help "Experiment start time")
