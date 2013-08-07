{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module System.Arte.BackendCBuffer where

import Foreign
import Foreign.C
import Foreign.C.ForeignPtr

#include <buffer.h>
#include <filter.h>
#include <spike.h>

data CircBuffer = CircBuffer { circData         :: Ptr CDouble
                             , circNChan        :: CInt
                             , circNSampPerChan :: CInt
                             , circFirstCursor  :: CInt
                             , circLastCursor   :: CInt
                             , circDt           :: CDouble
                             , circLastFreshT   :: CDouble
                             } deriving (Show)

foreign import ccall safe "buffer.c setup_circular_buffer"
  c_setup_circular_buffer :: CInt  -- ^ Number of samples per channel
                          -> CInt  -- ^ Number of channels
                          -> CDouble -- ^ dt per sample
                          -> IO (Ptr CircBuffer) -- ^ Result struct

foreign import ccall safe "buffer.c cleanup_circular_buffer"
  c_cleanup_circular_buffer :: Ptr CircBuffer -> IO ()

newCircBuffer :: Int -- ^ Number of samples per channel
              -> Int -- ^ Number of channels
              -> Double -- ^ dt per sample
              -> IO CircBuffer
newCircBuffer nSamp nChan dt = undefined

disposeCircBuffer :: CircBuffer -> IO ()
dispoceCircBuffer cBuf = alloca $ \ptr -> do
  poke ptr cBuf
  c_cleanup_circular_buffer ptr
  
  
