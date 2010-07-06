--------------------------------------------------------------------------------
-- |
-- Module      :  Data.BitVector
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

module Data.BitVector where

import Prelude hiding (Ord(..), Eq(..), (&&), (||), not, and, or, length, (++))
import qualified Prelude
import Data.Generics    ( Data, Typeable )
import Data.Bits (testBit, Bits)

import Data.Classes
import Data.Bit

-- --------------------

-- the internal representation of a BitVector
data BitVector
  = Vec [Bit]
  deriving (Prelude.Eq, Prelude.Ord, Data, Typeable)

-- --------------------
-- discussion of Endianness

data Endianness
  = LittleEndian
  | BigEndian
  deriving (Prelude.Eq)

-- changing this variable will not affect the functionality of this module in
-- any way (assuming we've defined it correctly); it only affects the internal
-- representation, not the interface to the outside world.  changing this could
-- have a slight impact on performance
internal_endianness :: Endianness
--internal_endianness = LittleEndian
internal_endianness = BigEndian

{- TODO update this documentation

Due to the nuances of the languages, endianness can mean something different in
Verilog and Haskell.  In fact, we have 3 different data types for which we need
to clearly define endianness.

  * Verilog bit-vectors - as dictated by the Verilog standard)

  * Haskell list of bits - anything of type [Bit] or Vector Bit

  * BitVector - what this module defines

In Verilog, words are always written with the least significant on the right.
That is, {0,0,1,0} corresponds to the number 2 regardless of endianness.

Endianness simply determines how we index into a word.  In little endian, index
0 is on the right and we count up from right ot left, and in big endian it is on
the left.  Thus, in the bit-vector {0,0,1,0}, the 1 is at index 1 if it is
little endian and index 2 if it is big endian.

The endianness of a bit-vector is determined by its declaration:

  wire [3:0] x;  // little endian
  wire [0:3] y;  // big endian

In this library, bit-vectors are represented internally as /big endian/ in the
sense that index 0 is the most significant (this is set by the
'internal_endianness' variable).  Externally, however, bit-vectors appear as
/little endian/ to the user, with the default index functions always indexing
from the right (least significant) and the alternate index functions (named *R)
indexing from the left (most significant).

The confusion occurs when we convert between a Haskell List or Vector and a
BitVector. In Haskell, Lists and Vectors follow the convention that they always
read from left to right and that index 0 is on the left.  For Haskell Lists and
Vectors, we define endianness based on the order of the bits, not how we index
into them.  Little endianness is when the left-most bit (index 0) is the least
significant, and big endianness is when the right-most bit is the least
significant.  Thus [F, F, T, F] represents the number 2 if it is /big-endian/
and the number 4 if it is /little-endian/.  Verilog bit-vectors always have the
least significant bit on the right, regardless of endianness.  Therefore, any
list of Bits should also have the least significant on the right, which by the
above definition would be /big-endian/.  The 'fromBits' and 'toBits' functions
expect and product /big-endian/ lists of bits.

In summary, endianness for the aforementioned types are as follows:

  * Verilog bit-vector
      - least significant is always on the right, regardless of endianness
      - endianness defines which side is index 0, and whether you count up or
        down when iterating from least to most significant.
      - [7:0] is little-endian, [0:7] is big-endian.

  * Haskell list of bits
      - index 0 is always on the left; that's baked into Haskell
      - big-endian is when least significant is on the left (index 0); this is
        what Verilog does, regardless of endianness
      - in this module, anything of type [Bit] is assumed to be big-endian

  * BitVector
      - internal representation is Vector Bit, where endianness is defined by
        the "Haskell list of bits" defintion

      - user chooses what endianness they want. if the user wants little-endian
        BitVector, use the right-indexing functions (indexR, sliceR); for
        big-endian BitVector, use the left indexing functions
        (indexL, sliceL).  all other functions are endianness-agnostic.
-}

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

-- convert a big-endian list of bits (where most significant bit is index 0, on
-- the left) to a BitVector.
fromBits :: [Bit] -> BitVector
fromBits = Vec . maybeReverse BigEndian

-- convert a BitVector to a big-endian list of bits.
toBits :: BitVector -> [Bit]
toBits (Vec bs) = maybeReverse BigEndian bs

maybeReverse :: Endianness -> [a] -> [a]
maybeReverse e xs
  | e == internal_endianness = xs
  | otherwise                = reverse xs

-- --------------------
-- utility functions

reverseIndex :: BitVector -> Int -> Int
reverseIndex (Vec v) i = Prelude.length v - i - 1

-- TODO handle negative numbers
neededBits :: (Integral a) => a -> Int
neededBits x
  = ceiling (logBase 2 (fromIntegral x+1))

{-
neededBits :: (Num a, Num b, Prelude.Ord a) => a -> b
neededBits x = fromIntegral (bitWidth x 1 (0::Integer))
  where
    bitWidth n p res
      | n < p = res
      | otherwise = bitWidth n (2*p) (res+1)
-}

-- bitsToNum and numToBits operate on a big-endian list of bits.
-- functions that use them will be more efficient when internal_endianness
-- is BigEndian, since they won't have to reverse the list.
bitsToNum :: forall a v . (SubType Bool v, Bits a, Num a) => [v] -> Maybe a
bitsToNum = f 0
    where f :: a -> [v] -> Maybe a
          f acc []     = Just acc
          f acc (x:xs) = case prj x of
                           Just b  -> let acc' = acc * 2 + (if b then 1 else 0)
                                      in acc' `seq` f acc' xs
                           Nothing -> Nothing

numToBits :: (SubType Bool v, Bits a, Num a) => Int -> a -> [v]
numToBits n i = map (inj . testBit i) [n-1, n-2..0]

-- pad: add 'n' 0s on the MSB end
pad :: Int -> BitVector -> BitVector
pad  = pad_internal LittleEndian

pad_internal :: Endianness -> Int -> BitVector -> BitVector
pad_internal e n (Vec bs)
  | e == internal_endianness  = Vec $ (Prelude.++) bs (Prelude.replicate n F)
  | otherwise                 = Vec $ (Prelude.++) (Prelude.replicate n F) bs

-- resize a bitvector so that its total length is n'.
-- this will either truncate or pad on the MSB end.
resize :: Int -> BitVector -> BitVector
resize  = resize_internal LittleEndian

resize_internal :: Endianness -> Int -> BitVector -> BitVector
resize_internal e n' x
  | n' < 0     = error "resize: size less than 0"
  | n' > n     = pad_internal e (n' - n) x
  | otherwise  = case x of
                   Vec bs
                     | e == internal_endianness
                     -> Vec $ take n' bs
                     | otherwise
                     -> Vec $ drop (n - n') bs
  where
    n = length x

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
(!) = indexR

indexR, indexL :: BitVector -> Int -> Bit
indexR = index_internal LittleEndian
indexL = index_internal BigEndian

index_internal :: Endianness -> BitVector -> Int -> Bit
index_internal e (Vec v) i
  | e == internal_endianness
  = (!!) v i
  | otherwise
  = (!!) v (Prelude.length v - i - 1)

sliceR, sliceL :: Int -> Int -> BitVector -> BitVector
sliceR = slice_internal LittleEndian
sliceL = slice_internal BigEndian

-- take a slice of width 'n' starting at index 'i'.
slice_internal :: Endianness -> Int -> Int -> BitVector -> BitVector
slice_internal e i n (Vec v)
  = error "TODO slice"
  {-Vec (V.slice i' n v)
  where
    i' | e == internal_endianness = i
       | otherwise                = V.length v - i - 1
  -}

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

plus'' :: BitVector -> BitVector -> BitVector
plus'' x y
  = fromBits $ f F (toBits (resize n x)) (toBits (resize n y))
  where
    n = 1 + max (length x) (length y)
    f _ [] []         = []
    f c (a:as) (b:bs) = let s       = a `xor` b `xor` c
                            c'      = (a && b) || (b && c) || (a && c)
                        in s : f c' as bs

arithOp :: Int -> (Integer -> Integer -> Integer)
        -> BitVector -> BitVector -> BitVector
arithOp n f x y
  = case (toNum x, toNum y) of
      (Just v1, Just v2) -> fromNum n (f v1 v2)
      _                  -> Vec (Prelude.replicate n U)

toNum :: Num a => BitVector -> Maybe a
toNum (Vec v)
  = case mapM prj v of
      Just bs -> let n = Prelude.length v
                     f i b = if b then 2^(n-1-i) else 0
                 in Just $ sum $ zipWith f [0..n-1] bs
      Nothing -> Nothing

fromNum :: (Integral a) => Int -> a -> BitVector
fromNum w x
  = fromBits (numToBits w (Prelude.toInteger x))

-- --------------------
