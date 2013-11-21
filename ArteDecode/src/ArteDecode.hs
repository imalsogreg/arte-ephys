module Arte.Decode where

import Data.Ephys.Net
import Data.Ephys.NetMessage
import Data.Ephys.Spike
import Data.Ephys.TrodeSpike

import Control.Concurrent.STM

main :: IO ()
main = do
  masterNode' <- getAppNode "master" Nothing
  spikeNodes  <- getAllSpikeNodes -- TODO define this here or in Net.hs
  