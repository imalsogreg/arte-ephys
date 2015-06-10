module Main where

import Options.Applicative
import GHC.Word
import System.Arte.Decode.Types

data Opts = Opts {
  port :: Word32
  } deriving (Eq, Show)

optStruct :: Parser Opts
optStruct = Opts
       <$> option auto
       (long "port"
        <> short 'p'
        <> help "Port to listen for position estimates")

masterOpts = info (helper <*> optStruct)
             (fullDesc
             <> progDesc "Decoder matser"
             <> header "master-decoder")

main :: IO ()
main = do
  opts <- execParser masterOpts
  print opts
