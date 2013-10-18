{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Arte.Common

import qualified System.ZMQ as ZMQ
import qualified Pipes as PP
import Data.Yaml
import System.Directory
import System.Environment
import System.Console.CmdArgs

data CArgs = CArgs { immediateStart    :: Bool
                   , mwlFileExtension  :: String
                   , arteFileExtension :: String
                   , baseDirectory     :: String
                   , searchDepth       :: Int
                   , baseNames         :: [String]
} deriving (Show, Data, Typeable)

cArgs = CArgs { immediateStart = def &= help "Stream immediately" &= opt False
              , mwlFileExtension = def &= opt "tt"
              , arteFileExtension = def &= opt "data"
              , baseDirectory = def &= opt "./"
              , searchDepth = def &= opt (0 :: Int)
              , baseNames = def &= opt ([] :: [String]) &= args
              }

main = print =<< cmdArgs cArgs
{-                     
main :: IO ()
main = do
  (Just netSettings) <- parse =<< readFile (getEnv "HOME")
  let (Just spikeSettings) = lookup netSettings "spkes"
      (myHostname myPortnum) = Vector.head spikeSettings
  fileNames <-
-}