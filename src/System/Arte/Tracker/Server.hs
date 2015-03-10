{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Arte.Tracker.Server where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Data.Attoparsec.ByteString.Char8 as A
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Network
import Safe
import System.IO
------------------------------------------------------------------------------
import System.Arte.Tracker.Types


------------------------------------------------------------------------------
serveFromSingleCamera :: ServerOptions -> TBMQueue RatPos -> IO ()
serveFromSingleCamera ServerOptions{..} frameQueue = do
  sock <- listenOn (PortNumber (fromIntegral serverOptsPort))

  -- Loop until thread is killed from main
  forever $ do
    (handle, host, port) <- accept sock
    putStrLn (unwords ["Accepted connection from", show host, ":", show port])
    forkFinally (talk handle frameQueue) (\_ -> hClose handle)

------------------------------------------------------------------------------
talk :: Handle -> TBMQueue RatPos -> IO ()
talk handle frameQueue = 
  undefined


------------------------------------------------------------------------------
toNetstring :: ToJSON a => NetstringMethod -> a -> Netstring
toNetstring nsMethod a
  | nsMethod == AngleBrackets =
    Netstring (BS.concat ["<", netInternals, ">"])
  | nsMethod == Bare = Netstring netInternals
  where
    netPayload    = BSL.toStrict (encode a)
    netPayloadLen = BS.pack (show (BS.length netPayload))
    netInternals  = BS.concat [netPayloadLen, ":", netPayload, ","]


fromNetstring :: FromJSON a => NetstringMethod -> Netstring
              -> Either String a
fromNetstring nsMethod (Netstring str) =
  let bsPayload = eitherResult (parse netParser str)
      netParser :: Parser BS.ByteString
      netParser = do
        when (nsMethod == AngleBrackets) (char '<' >> return ()) -- TODO: this right?
        len <- decimal
        let x = len :: Int
        char ':'
        pload <- A.take len
        char ','
        when (nsMethod == AngleBrackets) (char '>' >> return ())
        return pload
  in BSL.fromStrict <$> bsPayload >>= eitherDecode

  

    
