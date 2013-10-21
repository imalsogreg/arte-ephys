{-# LANGUAGE OverloadedStrings #-}

module Arte.Common.Network where

import Data.Text hiding (unwords)
import Data.Maybe (catMaybes)
import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Aeson
import Data.Yaml
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans.Class
import qualified Data.HashMap.Strict as HashMap

type HostName = String
type IPAddy   = String
type Port     = String

getAppNode :: String -> String -> FilePath -> IO (Either String Node)
getAppNode nodeType appName fn = do
  f <- BS.readFile fn
  let yObj =  Data.Yaml.decode fn :: Maybe Value
      os = yObj & catMaybes . toListOf (key nodeType . traverseArray) :: [Node]
  case Prelude.filter (\n -> nodeName n == appName) os of
    [] -> return $ Left (unwords ["Couldn't find",appName,"in",nodeType])
    (m:atches) -> return $ Right m

data Host = Host 
            { hostName :: Text
            , hostIP   :: String
            } deriving (Show)
                       
a :: Node
a = Node "sampleNodeName" (Host "HostName" "HostIP") "nodePort"

data Node = Node 
            {  nodeName :: Text
            ,  nodeHost :: Host
            ,  nodePort :: String
            }

instance FromJSON Host where
  parseJSON (Object v) = Host <$> 
                         v .: ("name" :: Text) <*>
                         v .: "ip"
  parseJSON _  = mzero
  
instance FromJSON Node where
  parseJSON (Object v) = Node <$>
                         v .: "name" <*>
                         v .: "host" <*>
                         v .: "port"
  parseJSON _          = mzero


