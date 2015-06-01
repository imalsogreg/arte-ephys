module Types where

import GHC.Word
import Control.Applicative
import Options.Applicative

data OutputFormat = ArteOld
                  | ArteNew
                  deriving (Eq, Show, Read)

data DataSourceOpts = DataSourceOpts
                      { outputFormat :: OutputFormat
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
                 <> help "Write to network in arteold or artenew format")
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
