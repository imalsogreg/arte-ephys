{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Arte.Common.Network where

import Data.Text hiding (unwords)
import Control.Applicative
import Control.Monad
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Yaml
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map, keys, member)
import System.Environment (lookupEnv)

type HostName = String
type IPAddy   = String
type Port     = String

data Host = Host 
            { _hostIP   :: String
            } deriving (Eq, Show)

$(makeLenses ''Host)

data Node = Node 
            {  _nodeHost :: Host
            ,  _nodePort :: Int
            } deriving (Eq, Show)

$(makeLenses ''Node)

getAppNode :: String -> Maybe FilePath -> IO (Either String Node)
getAppNode nodeName fn' = do
  fn <- netConfOrDefaultPath fn'
  f <- BS.readFile fn
  let v =  Data.Yaml.decode f :: Maybe Value
  case v ^. key "nodes" . key (pack nodeName) :: Maybe Node of
    Just node -> return $ Right node

  -- Error handling
    Nothing->
      case v of
        Nothing -> return $ Left ("Bad conf file: " ++ fn)
        _ -> case v ^. key "nodes" :: Maybe (Map String Node) of
          Nothing      ->
            return $ Left ("Malformed conf file lacks nodes: field")
          Just nodeMap -> case member nodeName nodeMap of
            True  -> return $ Left ("No parse for node " ++ nodeName)
            False -> return $ Left
                     ("Node '" ++ nodeName ++ "' not in node list: " ++
                      (unwords . keys  $ nodeMap))

netConfOrDefaultPath :: Maybe FilePath -> IO FilePath
netConfOrDefaultPath (Just fp) = return fp
netConfOrDefaultPath Nothing   = do
  homeName <- lookupEnv "HOME"
  case homeName of
    Nothing -> error "Couldn't find $HOME"
    Just h  -> return $ h ++ "/.arte-ephys/network.conf"
  
  


instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "ip"
  parseJSON _  = mzero
  
instance FromJSON Node where
  parseJSON (Object v) = Node <$>
                         v .: "host" <*>
                         v .: "port"
  parseJSON _          = mzero

instance ToJSON Host where
  toJSON (Host hIP) =
    object ["ip"   .= hIP]

instance ToJSON Node where
  toJSON (Node nHost nPort) =
    object ["host" .= nHost
           ,"port" .= nPort]
