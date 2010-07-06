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

import Data.Bits
import Data.BitVector.Util
import Data.Classes
import Data.Endianness

----------------------------------------

bv_endianness :: Endianness
bv_endianness = LittleEndian

----------------------------------------

bitsToNum :: forall a v . (SubType Bool v, Bits a, Num a) => [v] -> Maybe a
bitsToNum vs = f 0 0 vs
  where
    f :: Int -> a -> [v] -> Maybe a
    f _ acc []      = Just acc
    f ix acc (x:xs) = case prj x of
                        Just b -> let acc' = if b then setBit acc ix else acc
                                  in acc' `seq` f (ix+1) acc' xs
                        _      -> Nothing

numToBits :: (SubType Bool v, Bits a, Num a) => Int -> a -> [v]
numToBits n i = map (inj . testBit i) [0..n-1]

----------------------------------------
