module Main where

import Arte.Common.Net
import Arte.Common.NetMessage
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

type SpikeHistory = Int -- Placeholder.  Will be more like: 

newPlaceCell :: Track
             -> TVar Tetrodes
             -> TrodeName
             -> ClusterMethod
             -> IO (Maybe PlaceCell)
newPlaceCell track tetrodesV t cMethod = do
  tetrodes <- atomically $ readTVar tetrodesV
  case Map.lookup tetrodes t of
    Nothing -> return Nothing
    Just (_, spikeHistoryV) -> do
      return . Just $
        PlaceCell cMethod (Map.fromList [(p,0)|p <- allTrackPos track])
        -- TODO: Build from spike history!

setTetrodeCluster :: Track
                  -> TVar Tetrodes 
                  -> TrodeName
                  -> ClusterName
                  -> IO ()
setTetrodeClusters track tetrodesV t c clustMap = do
  tetrodes <- atomically $ readTVar tetrodesV  -- atomically here, happy to
  let tetrode' = case Map.lookup tetrodes t of     -- drop a few spikes while 
        Just (placeCellsV,sHistoryV) -> do     -- updating cluster definition
          (placeCells,sHistory) <- atomically $ do
            p <- readTVar placeCellsV
            s <- readTVar sHistoryV
            return (p,s)
          case Map.lookup placeCells c of
            Nothing ->
              let newCell = 
          
  
type TetrodeName = Int
type ClusterName = Int

type Tetrode = (TVar (Map.Map Int PlaceCell), TVar SpikeHistory)

type Tetrodes = Map.Map TetrodeName Tetrode

handleRequests :: TQueue ArteMessage -> TVar Tetrodes -> IO ()
handleRequests queue tetrodes = do
  (ArteMessage t nFrom nTo mBody) <- atomically $ readTQueue queue

main :: IO ()
main = do
  masterNode' <- getAppNode "master" Nothing
  spikeNodes  <- getAllSpikeNodes    Nothing
  print "Ok"