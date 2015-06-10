{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Traversable
import Options.Applicative
import GHC.Word
import Control.Concurrent.STM
import System.Arte.Decode.Types
import Data.Ephys.EphysDefs
import qualified Data.Map as Map
import Data.Ephys.Position
import Data.Ephys.TrackPosition

data Opts = Opts {
    port :: Word32
  , rTau :: Double
  } deriving (Eq, Show)

optStruct :: Parser Opts
optStruct = Opts
       <$> option auto
       (long "port"
        <> short 'p'
        <> help "Port to listen for position estimates")
       <*> option auto
       (long "rTau"
       <> short 'r'
       <> help "Decoding interval rTau (seconds)")

masterOpts = info (helper <*> optStruct)
             (fullDesc
             <> progDesc "Decoder matser"
             <> header "master-decoder")

main :: IO ()
main = do
  opts <- execParser masterOpts
  
  print opts

data MasterState = MasterState {
    tetrodeEstimate :: Map.Map TrodeName (TVar (Field))
  , ratPos          :: TVar Position
  , masterEstimate  :: TVar Field
  }


------------------------------------------------------------------------------
showMasterState :: MasterState -> IO String
showMasterState MasterState{..} =
  (\tEs p mE ->
     unlines [ "MasterState {", show tEs, show p, show mE, "}"])
  <$> traverse readTVarIO tetrodeEstimate
  <*> readTVarIO ratPos
  <*> readTVarIO masterEstimate

