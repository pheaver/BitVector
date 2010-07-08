--------------------------------------------------------------------------------
-- |
-- Module      :  Data.BitVector.BitVector1
-- Copyright   :  (c) 2010 Philip Weaver
-- License     :  BSD3
--
-- Maintainer  :  philip.weaver@gmail.com
-- Stability   :  provisional
-- Portability :  Haskell 98
--
-- (Description)
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# OPTIONS_DERIVE --append #-}

module Data.BitVector.BitVector1 where

import Prelude hiding (Ord(..), Eq(..), (&&), (||), not, and, or, length, (++))
import qualified Prelude
import Data.Generics    ( Data, Typeable )

import Data.Bit
import Data.BitVector.Util
import Data.BitVector.UtilBE
import Data.Boolean
import Data.Compare
import Data.Endianness

-- --------------------

-- the internal representation of a BitVector
newtype BitVector
  = Vec [Bit]
  deriving (Prelude.Eq, Prelude.Ord, Data, Typeable)

-- --------------------

-- changing this variable will not affect the functionality of this module in
-- any way (assuming we've defined it correctly); it only affects the internal
-- representation, not the interface to the outside world.  changing this could
-- have a slight impact on performance
{-# INLINE internal_endianness #-}
internal_endianness :: Endianness
internal_endianness = LittleEndian
--internal_endianness = BigEndian

-- --------------------
-- conversion utility functions.
-- printing to string and converting to and from [Bit].

-- TODO
-- pretty print - take in base, yield [Doc] so user can insert punctuation

showBitVector :: Endianness -> (Bit -> Char) -> BitVector -> String
showBitVector e f (Vec bs) = map f (maybeReverse e bs)

instance Show BitVector where
  show = showBitVector BigEndian f
    where f T = '1'
          f F = '0'
          f U = 'X'
          f Z = 'Z'

{-# INLINE fromBits #-}
fromBits :: [Bit] -> BitVector
fromBits = Vec

{-# INLINE toBits #-}
toBits :: BitVector -> [Bit]
toBits (Vec bs) = bs

-- convert a big-endian list of bits (where most significant bit is index 0, on
-- the left) to a BitVector.
fromBitsBE :: [Bit] -> BitVector
fromBitsBE = Vec . maybeReverse BigEndian

fromBitsLE :: [Bit] -> BitVector
fromBitsLE = Vec . maybeReverse LittleEndian

-- convert a BitVector to a big-endian list of bits.
toBitsBE :: BitVector -> [Bit]
toBitsBE (Vec bs) = maybeReverse BigEndian bs

toBitsLE :: BitVector -> [Bit]
toBitsLE (Vec bs) = maybeReverse LittleEndian bs

maybeReverse :: Endianness -> [a] -> [a]
maybeReverse e xs
  | e == internal_endianness = xs
  | otherwise                = reverse xs

-- --------------------
-- utility functions

reverseIndex :: BitVector -> Int -> Int
reverseIndex (Vec v) i = Prelude.length v - i - 1

-- pad: add 'n' 0s on the MSB end
pad :: Int -> BitVector -> BitVector
pad n (Vec bs)
  = Vec $ case internal_endianness of
            LittleEndian -> (Prelude.++) bs (Prelude.replicate n F)
            BigEndian    -> (Prelude.++) (Prelude.replicate n F) bs

-- resize a bitvector so that its total length is n'.
-- this will either truncate or pad on the MSB end.
resize :: Int -> BitVector -> BitVector
resize n' v
  | n' < 0     = error "resize: size less than 0"
  | n' > n     = pad (n' - n) v
  | otherwise  = takeR n' v
  where
    n = length v

-- makeSameLength xs
--   = map (resize n) xs
--   where n = maximum (map length xs)

-- --------------------
-- initialization

empty :: BitVector
empty = Vec []

singleton :: Bit -> BitVector
singleton b = Vec [b]

replicate :: Int -> Bit -> BitVector
replicate n b = Vec (Prelude.replicate n b)

-- generate :: Int -> (Int -> Bit) -> BitVector
-- generate =

-- --------------------
-- concatenation

-- cons :: Bit -> BitVector -> BitVector
-- snoc :: BitVector -> Bit -> BitVector

(++) :: BitVector -> BitVector -> BitVector
(++) (Vec v1) (Vec v2)
  = Vec $ case internal_endianness of
            BigEndian    -> v1 Prelude.++ v2
            LittleEndian -> v2 Prelude.++ v1

concat :: [BitVector] -> BitVector
concat = foldr (++) empty

-- --------------------
-- length information

{-# INLINE length #-}
length :: BitVector -> Int
length (Vec v)   = Prelude.length v

{-# INLINE null #-}
null :: BitVector -> Bool
null (Vec v)     = Prelude.null v

-- --------------------
-- indexing.
-- functions named *R index from the right (little-endian).
-- functions named *L index from the left (big-endian).

(!) :: BitVector -> Int -> Bit
(!) = flip indexR

indexR, indexL :: Int -> BitVector -> Bit
indexR i (Vec v)
  = case internal_endianness of
      LittleEndian -> v !! i
      BigEndian    -> v !! (Prelude.length v - i -1)

indexL i (Vec v)
  = case internal_endianness of
      LittleEndian -> v !! (Prelude.length v - i -1)
      BigEndian    -> v !! i

-- TODO throw error if out of range?
sliceR, sliceL :: Int -> Int -> BitVector -> BitVector

-- [i-:n]
sliceR i n = takeL n . takeR (i+1)

-- [i+:n]
sliceL i n = takeL n . dropL i

-- TODO throw error if out of range?
takeL, takeR, dropL, dropR :: Int -> BitVector -> BitVector
takeL i (Vec bs)
  = Vec $ case internal_endianness of
            LittleEndian -> drop (Prelude.length bs - i) bs
            BigEndian    -> take i bs

takeR i (Vec bs)
  = Vec $ case internal_endianness of
            LittleEndian -> take i bs
            BigEndian    -> drop (Prelude.length bs - i) bs

dropL i (Vec bs)
  = Vec $ case internal_endianness of
            LittleEndian -> take (Prelude.length bs - i) bs
            BigEndian    -> drop i bs

dropR i (Vec bs)
  = Vec $ case internal_endianness of
            LittleEndian -> drop i bs
            BigEndian    -> take (Prelude.length bs - i) bs

-- --------------------
-- specialized folds

and :: BitVector -> Bit
and (Vec v)
  = foldr (&&) T v

or :: BitVector -> Bit
or (Vec v)
  = foldr (||) F v

-- --------------------
-- TODO arithmetic

instance Num BitVector where
  -- TODO handle signs correctly
  fromInteger i = fromNum (neededBits i + 1) i
                  -- leave room for sign - this is what Verilog does

  (+) x y = arithOp (1 + max (length x) (length y)) (+) x y
  (-) x y = arithOp (1 + max (length x) (length y)) (-) x y
  (*) x y = arithOp (length x + length y) (*) x y

-- unsignedAdd :: BitVector -> BitVector -> BitVector

plus :: BitVector -> BitVector -> BitVector
plus x y
  = arithOp (1 + max (length x) (length y)) (+) x y

-- handle case where argument is partially defined, by yielding a partially
-- defined result
plus' :: BitVector -> BitVector -> BitVector
plus' x y
  = fromBits $ snd $ f (toBits (resize n x)) (toBits (resize n y))
  where
    n = 1 + max (length x) (length y)
    f [] []         = (F, [])
    f (a:as) (b:bs) = let s       = a `xor` b `xor` c
                          c'      = (a && b) || (b && c) || (a && c)
                          (c, ss) = f as bs
                      in (c', s : ss)
    f _ _ = error "plus'"

plus'' :: BitVector -> BitVector -> BitVector
plus'' x y
  = fromBits $ f F (toBits (resize n x)) (toBits (resize n y))
  where
    n = 1 + max (length x) (length y)
    f _ [] []         = []
    f c (a:as) (b:bs) = let s       = a `xor` b `xor` c
                            c'      = (a && b) || (b && c) || (a && c)
                        in s : f c' as bs
    f _ _ _ = error "plus''"

arithOp :: Int -> (Integer -> Integer -> Integer)
        -> BitVector -> BitVector -> BitVector
arithOp n f x y
  = case (toNum x, toNum y) of
      (Just v1, Just v2) -> fromNum n (f v1 v2)
      _                  -> Vec (Prelude.replicate n U)

toNum :: Num a => BitVector -> Maybe a
toNum (Vec v)
  = case mapM toBool v of
      Just bs -> let n = Prelude.length v
                     f i b = if b then 2^(n-1-i) else 0
                 in Just $ sum $ zipWith f [0..n-1] bs
      Nothing -> Nothing

fromNum :: (Integral a) => Int -> a -> BitVector
fromNum w x
  = fromBits (numToBits w (Prelude.toInteger x))

-- --------------------
