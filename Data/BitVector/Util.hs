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

-- TODO handle negative numbers
neededBits :: (Integral a) => a -> Int
neededBits x
  = ceiling (logBase 2 (fromIntegral x+1))
