module Main where

import System.Arte.ArteBase

import qualified System.ZMQ as ZMQ
import qualified Pipes as PP
import Data.Yaml
import System.Directory
import System.Environment
import System.Console.CmdArgs

data CArgs = CArgs { mwlExtensions  :: [String]
                   , arteExtensions :: [String]
                   , baseNames      :: Maybe [String]
                   , 

main :: IO ()
main = do
  (Just netSettings) <- parse =<< readFile (getEnv "HOME")
  let (Just spikeSettings) = lookup netSettings "spkes"
      (myHostname myPortnum) = Vector.head spikeSettings
  fileNames <- 