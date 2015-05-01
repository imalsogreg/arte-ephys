module Main where

--------------------------------------------------------------------------------
import System.Random
import qualified Data.Map as Map
import Control.DeepSeq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
------------------------------------------------------------------------------
import Criterion
import Criterion.Main
import Criterion.IO
import Data.Ephys.EphysDefs
import Data.Ephys.TrackPosition
import Data.Ephys.Cluster
import Data.Ephys.Spike
--------------------------------------------------------------------------------
import DecodeAlgo
import DecoderState


--------------------------------------------------------------------------------
main :: IO ()
main = newStdGen >>= defaultMain . benchmarks

benchmarks :: StdGen -> [Benchmark]
benchmarks r =
  [bgroup "reconstruction"
    [bench "reconstruct1cell" $
     nf (clusteredReconstruction rTauSec field1 count1) occ
    ,bench "reconstruct32cell" $
     nf (clusteredReconstruction rTauSec field32 count32) occ
    ,bench "reconstruct64cell" $
     nf (clusteredReconstruction rTauSec (field32 ++ field32)
         (count32 ++ count32)) occ
    ]
    ,bgroup "spikesort"
     [bench "sort1poly" $ nf (spikeInCluster (clusterMethods !! 1)) trodeSpike
     ,bench "sort3poly" $ nf (spikeInCluster (clusterMethods !! 0)) trodeSpike
     ]
    ,bgroup "updateField"
     [bench "updateField" $ nf (updateField (+) occ) occ]
  ]

rTauSec = 0.02
field1  = [Map.fromList [(tp,0.1) | tp <- allTrackPos track]]
field32 = take 32 . repeat $ Map.fromList [(tp,0.1) | tp <- allTrackPos track]
occ     = head field1
count1  = [1]
count32 = [1..32]

-- Taken from caillou 112812clip2 tetrode 0928 cbfile-run
clusterMethods :: [ClusterMethod]
clusterMethods =
  [ClustIntersection
                 [ClustCartBound $ CartBound 0 2 [(385.045,   233.038)
                                     ,(303.145,   190.074)
                                     ,(248.545,   132.341)
                                     ,(251.275,   63.8672)
                                     ,(349.555,   89.377 )
                                     ,(427.36,    172.62 )
                                     ]
                 ,ClustCartBound $ CartBound 0 1 [(361.84,   246.464)
                                      ,(263.56,   199.472)
                                      ,(247.18,   130.998)
                                      ,( 260.83,  81.3213)
                                      ,(323.62,   90.7197)
                                      ,(415.075,  202.157)
                                      ]
                 , ClustCartBound $ CartBound 0 3 [(385.045, 359.244)
                                      ,(301.78,  289.428)
                                      ,(249.91,  215.584)
                                      ,(254.005, 177.99 )
                                      ,(355.015, 238.408)
                                      ,(443.74,  329.707)
                                      ]
                 ]
                    
                , ClustCartBound $ CartBound 0 1 [(281.305, 296.141 )
                                     ,(178.93,  231.695 )
                                     ,(124.33,  143.082 )
                                     ,(136.615, 109.516 )
                                     ,(169.375, 118.915 )
                                     ,(267.655, 220.954 )
                                     ,(290.86,  284.057)]
                ]
                                       

trodeSpike :: TrodeSpike
trodeSpike = TrodeSpike 0 0 0 $
             V.fromList [U.fromList [0,0,0,1,2,3,4,50,200,50,40,30,20,10,5,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
                        ,U.fromList [0,0,0,1,2,3,4,50,200,50,40,30,20,10,5,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
                        ,U.fromList [0,0,0,1,2,3,4,50,200,50,40,30,20,10,5,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
                        ,U.fromList [0,0,0,1,2,3,4,50,200,50,40,30,20,10,5,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
                        ]

instance NFData TrackPos where
  rnf m = m `seq` ()
