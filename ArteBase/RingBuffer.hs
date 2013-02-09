----------------------------------------------------------------------
-- |
-- Module      : DaqsConf 
-- Copyright   : (c) Greg Hale
-- License     : GPL-3
-- 
-- Author      : John Lato
-- Maintainer  : Greg Hale <imalsogreg@gmail.com>
-- Stability   : unstable 
-- Portability :
--
-- Coppied from John Lato's blog post on functional circular buffers
-- http://johnlato.blogspot.com/2011/07/circular-buffers.html
----------------------------------------------------------------------



module RingBuffer (
    RingBuffer
     ,new
     ,newInit
     ,push
     ,length
     ,(!)
     )  where

import Prelude hiding (length)

import qualified Data.Sequence as S

newtype RingBuffer a = RB (S.Seq a) deriving (Eq, Ord, Show)

-- | Create a new RingBuffer, initialized to all 0's, of the given size
new :: (Num a) => Int -> RingBuffer a
new = newInit 0
{-# INLINE new #-}

-- | Create a new RingBuffer from a given initial value
newInit :: a -> Int -> RingBuffer a
newInit i sz | sz <= 0 = error "can't make empty ringbuffer"
newInit i sz           = RB (S.replicate sz i)
{-# INLINE newInit #-}

-- | Get the total size of a RingBuffer.
length :: RingBuffer a -> Int
length (RB vec) = S.length vec
{-# INLINE length #-}

-- | Look up a value in a RingBuffer.
(!) :: RingBuffer a -> Int -> a
(!) (RB vec) = S.index vec
{-# INLINE (!) #-}

-- | Push a new value into a RingBuffer.  The following will hold:
--     NewRingBuffer ! 0 === added element
--     NewRingBuffer ! 1 === OldRingBuffer ! 0
push :: RingBuffer a -> a -> RingBuffer a
push (RB vec) el   = case S.viewr vec of
    v' S.:> _ -> RB $ el S.<| v'
    _         -> error "internal error"
{-# INLINE push #-}