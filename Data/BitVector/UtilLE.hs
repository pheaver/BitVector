--------------------------------------------------------------------------------
-- |
-- Module      :  Data.BitVector.UtilLE
-- Copyright   :  (c) 2010 Philip Weaver
-- License     :  BSD3
--
-- Maintainer  :  philip.weaver@gmail.com
-- Stability   :
-- Portability :
--
-- (Description)
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Data.BitVector.UtilLE
  ( module Data.BitVector.Util,
    bv_endianness,
    bitsToNum, numToBits
  ) where

import Data.Bit
import Data.Bits
import Data.BitVector.Util
import Data.Endianness

----------------------------------------

bv_endianness :: Endianness
bv_endianness = LittleEndian

----------------------------------------

-- TODO make these work for any Boolean type (Bool, Bit, etc.)

bitsToNum :: forall a . (Bits a, Num a) => [Bit] -> Maybe a
bitsToNum vs = f 0 0 vs
  where
    f :: Int -> a -> [Bit] -> Maybe a
    f _ acc []      = Just acc
    f ix acc (x:xs) = case toBool x of
                        Just b -> let acc' = if b then setBit acc ix else acc
                                  in acc' `seq` f (ix+1) acc' xs
                        _      -> Nothing

numToBits :: (Bits a, Num a) => Int -> a -> [Bit]
numToBits n i = map (fromBool . testBit i) [0..n-1]

----------------------------------------
