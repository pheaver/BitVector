--------------------------------------------------------------------------------
-- |
-- Module      :  Data.BitVector.Util
-- Copyright   :  (c) 2010 Philip Weaver
-- License     :  BSD3
--
-- Maintainer  :  philip.weaver@gmail.com
-- Stability   :
-- Portability :
--
-- (Description)
--------------------------------------------------------------------------------

module Data.BitVector.Util where

import Data.Bits (Bits, (.&.))

----------------------------------------

-- determine the minimum number of bits needed to represent 'x'.
-- TODO handle negative numbers.
-- {-# INLINE neededBits #-}
neededBits :: (Integral a) => a -> Int
neededBits x
  = ceiling (logBase 2 (fromIntegral (x+1)))

-- find the index of the highest bit that is set
-- {-# INLINE msbIndex #-}
msbIndex :: (Integral a) => Int -> a -> Int
msbIndex w 0 = w
msbIndex _ x = floor (logBase 2 (fromIntegral x))

-- mask the upper bits of a number so that only the lower 'n' bits are set.
{-# INLINE maskWidth #-}
maskWidth :: (Bits a, Integral a) => Int -> a -> a
maskWidth w x = x .&. (2^w-1)
--maskWidth w x = mod x (2^w)

----------------------------------------
