{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveGeneric #-}

module Arte.Common.Net where

import Data.Text hiding (unwords,filter,head)
import Control.Applicative
import Control.Monad
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Yaml
import Control.Lens.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map, keys, member)
import System.Environment (lookupEnv)
import Network
import System.IO
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified System.ZMQ as ZMQ
import Text.Printf
import qualified Data.Serialize as S


type IPAddy   = String
type Port     = Int

data Host = Host 
            { _ip   :: IPAddy
            } deriving (Eq, Show)
$(makeLenses ''Host)

data Node = Node 
            { _host :: Host
            , _port :: Int
            , _inPort   :: Maybe Int
            } deriving (Eq, Show)
$(makeLenses ''Node)

data NetConfig = NetConfig
                 { _hosts :: Map.Map String Host
                 , _nodes :: Map.Map String Node
                 } deriving (Eq, Show)
$(makeLenses ''NetConfig)

withMaster :: Node -> ((Handle,ZMQ.Socket ZMQ.Sub) -> IO a) -> IO a
withMaster masterNode f = case masterInPort' of
    Nothing           -> error "Bad configuration file - master has no inPort."
    Just masterInPort -> do

      -- Connection to master 'in' port
      printf "Connecting to %s %s\n" (masterNode^.host.ip) (show masterInPort)
      hToMaster <- connectTo (masterNode ^. host.ip)
                   --(Service $ show masterInPort)
                   (PortNumber . fromIntegral $ masterInPort)
      printf "Successfully connected to masterInPort"
      
      -- Subscription to master 'pub' port
      printf "Connecting to master pub port %s %s\n" masterIP (show masterInPort)
      ZMQ.withContext 1 $ \ctx -> do
        ZMQ.withSocket ctx ZMQ.Sub $ \hFromMaster -> do
          let pubStr = "tcp://" ++ masterIP ++ ":" ++ show masterPubPort
          print $ "About to connect to pubport: " ++ pubStr
          ZMQ.connect hFromMaster pubStr
          print $ "About to subscribe to pubport"
          ZMQ.subscribe hFromMaster ""
          print $ "Subscribed, running function argument"
          f (hToMaster,hFromMaster)
  where masterIP      = masterNode^.host.ip
        masterPubPort = masterNode^.port
        masterInPort' = masterNode^.inPort
        
getAppNode :: String -> Maybe FilePath -> IO (Either String Node)
getAppNode name fn' = do
  fn <- netConfOrDefaultPath fn'
  f <- BS.readFile fn
  case decodeEither f of
    Left e -> return . Left $ unwords ["Couldn't parse file",fn,":",e]
    Right netConf -> case Map.lookup name (netConf^.nodes) of
      Nothing -> return . Left $ unwords ["Didn't find node",name,"in",fn]
      Just n -> return $ Right n

netConfOrDefaultPath :: Maybe FilePath -> IO FilePath
netConfOrDefaultPath (Just fp) = return fp
netConfOrDefaultPath Nothing   = do
  homeName <- lookupEnv "HOME"
  case homeName of
    Nothing -> error "Couldn't find $HOME"
    Just h  -> return $ h ++ "/.arte-ephys/network.conf"

sendWithSize :: (S.Serialize a) => Handle -> a -> IO ()
sendWithSize h a = do
  let a'     = S.encode a
      a'size = BS.length a'
  BS.hPut h (S.encode a'size)
  BS.hPut h a'
  hFlush h

receiveWithSize :: (S.Serialize a) => Handle -> IO (Either String a)
receiveWithSize h = do
  let intEncodedSize = BS.length (S.encode (1 :: Int))
  s' <- BS.hGet h intEncodedSize
  case S.decode s' of
    Left e  -> return . Left $ "Couldn't decode to a size. " ++ e
    Right s -> do
      a' <- BS.hGet h s
      case S.decode a' of
        Left e  -> return . Left $ "Couldn't decode a value." ++ e
        Right a -> return $ Right a

instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "ip"
  parseJSON _  = mzero

instance ToJSON Host where
  toJSON (Host hIP) =
    object ["ip"   .= hIP]

instance FromJSON Node where
  parseJSON (Object v) = Node <$>
                         v .:  "host" <*>
                         v .:  "port" <*>
                         v .:? "inPort"
  parseJSON _          = mzero

instance ToJSON Node where
  toJSON (Node nHost nPort iPort) =
    object (["host" .= nHost
           ,"port" .= nPort] ++ inPortElem)
    where inPortElem = case iPort of 
            Nothing -> [] 
            Just p -> ["inPort" .= p]


instance FromJSON NetConfig where
  parseJSON (Object v) = NetConfig <$>
                         v .: "hosts" <*>
                         v .: "nodes"
  parseJSON _          = mzero

instance ToJSON NetConfig where
  toJSON (NetConfig hs ns) =
    object ["hosts" .= hs
           ,"nodes" .= ns]

type ZmqHost    = String
type ZmqPort    = String
type ZmqSockStr = String
data ZmqProtocol = Tcp | Ipc deriving (Eq)

instance Show ZmqProtocol where
  show Tcp = "tcp"
  show Ipc = "ipc"

zmqStr :: ZmqProtocol -> ZmqHost -> ZmqPort -> ZmqSockStr
zmqStr prot h p = 
  show prot ++ "://" ++ h ++ ":" ++ p
