--------------------------------------------------------------------------------
-- |
-- Module      :  Data.BitVector.UtilBE
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

module Data.BitVector.UtilBE
  ( module Data.BitVector.Util,
    bv_endianness,
    bitsToNum, numToBits
  ) where

import Data.Bits
import Data.BitVector.Util
import Data.Classes
import Data.Endianness

{- TODO
 - pretty-print bitvector with choice of show function, base, punctuation.
 - index, slice, pad, truncate, resize, drop, take
 -
-}

----------------------------------------

bv_endianness :: Endianness
bv_endianness = BigEndian

----------------------------------------

bitsToNum :: forall a v . (SubType Bool v, Bits a, Num a) => [v] -> Maybe a
bitsToNum = f 0
  where
    f :: a -> [v] -> Maybe a
    f acc []     = Just acc
    f acc (x:xs) = case prj x of
                     Just b -> let acc' = acc * 2 + (if b then 1 else 0)
                               in acc' `seq` f acc' xs
                     _      -> Nothing
{-
bitsToNum vs = f (length vs-1) 0 vs
  where
    f :: Int -> a -> [v] -> Maybe a
    f _ acc []      = Just acc
    f ix acc (x:xs) = case prj x of
                        Just b -> let acc' = if b then setBit acc ix else acc
                                  in acc' `seq` f (ix-1) acc' xs
                        _      -> Nothing
-}

numToBits :: (SubType Bool v, Bits a, Num a) => Int -> a -> [v]
numToBits n i = map (inj . testBit i) [n-1, n-2..0]

----------------------------------------
