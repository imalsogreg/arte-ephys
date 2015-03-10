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
import qualified Data.ByteString.Char8 as BSC
import Network
import Safe
import System.IO
------------------------------------------------------------------------------
import System.Arte.Tracker.Types


------------------------------------------------------------------------------
serveFromSingleCamera :: ServerOptions -> Chan RatPos -> IO ()
serveFromSingleCamera ServerOptions{..} posChan = do
  sock <- listenOn (PortNumber (fromIntegral serverOptsPort))
  go sock posChan
    where go sk ch = do
            (handle, host, port) <- accept sk
            putStrLn (unwords ["Accepted connection from", show host, ":", show port])
            posChan' <- dupChan posChan
            forkFinally (talk handle posChan) (\_ -> hClose handle)
            go sk posChan'


------------------------------------------------------------------------------
talk :: Handle -> Chan RatPos -> IO ()
talk handle posChan = go
  where go = do
          p <- readChan posChan
          let msg = getByteString (toNetstring p)
          BSC.putStrLn $ BSC.concat ["Sending ", msg, " to ", BSC.pack (show handle)]
          BSC.hPut handle (getByteString (toNetstring p))
          hFlush handle
          go
  


------------------------------------------------------------------------------
toNetstring :: ToJSON a => a -> Netstring
toNetstring a = Netstring netInternals
  where
    netPayload    = BSL.toStrict (encode a)
    netPayloadLen = BSC.pack (show (BSC.length netPayload))
    netInternals  = BSC.concat [netPayloadLen, ":", netPayload, ","]


fromNetstring :: FromJSON a => Netstring -> Either String a
fromNetstring (Netstring str) =
  let bsPayload = eitherResult (parse netParser str)
      netParser :: Parser BSC.ByteString
      netParser = decimal >>= \n ->
                              char ':'
                              *> A.take n
                              <* char ','
  in BSL.fromStrict <$> bsPayload >>= eitherDecode

  

    
