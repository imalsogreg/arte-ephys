module Main where

import Arte.Common.Net
import Arte.Common.NetMessage
import Data.Ephys.Spike
import Data.Ephys.Spike
import Data.Ephys.Cluster
import Data.Ephys.PlaceCell

import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent.STM
import Data.Either

getAllSpikeNodes :: Maybe FilePath -> IO [Node]
getAllSpikeNodes configFilePath = 
  forM  ['A'..'Z'] 
  (\l -> getAppNode ("spikes" ++ [l]) configFilePath) >>= \nodes' ->
  return $ rights nodes'

--   All tetrodes       Name         ClustName AccumField'     ClusterBound'
--   |                  |            |         |               |
type Tetrodes = Map.Map Int (Map.Map Int (TVar PlaceCell, TVar ClusterMethod))

main :: IO ()
main = do
  masterNode' <- getAppNode "master" Nothing
  spikeNodes  <- getAllSpikeNodes    Nothing
  print "Ok"