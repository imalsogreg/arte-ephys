{-# LANGUAGE OverloadedStrings #-}

module Arte.Common.Network where

import Data.Text
import Control.Applicative
import Control.Monad
import Data.Yaml
import Data.Yaml.YamlLight
import Data.Yaml.YamlLight.Lens
import Control.Lens
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans.Class
import qualified Data.HashMap.Strict as HashMap

type HostName = String
type IPAddy   = String
type Port     = String

getAppNode :: String -> String -> FilePath -> IO (Either String Node)
getAppNode nodeType appName fn = do
  yObj <- parseYamlFile fn
  case yObj ^? key "spikes" of
    Nothing    -> return $ Left "Malformed network config file."
    Just (YSeq ys) ->
      case Prelude.filter (\y -> y ^? key "name" == Just (YStr $ BS.pack appName)) ys of
        [] -> return $ Left ("No node found named " ++ appName)
        matches -> case matches ^? nth 0 . _Yaml of
          Just node -> return $ Right node
          Nothing   -> return $ Left "Couldn't decode node."
--        matches -> return $ parseEither (parseJSON $ Prelude.head matches)

data Host = Host 
            { hostName :: Text
            , hostIP   :: String
            } deriving (Show)
                       
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


