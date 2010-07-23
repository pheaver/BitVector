--------------------------------------------------------------------------------
-- |
-- Module      :  Data.BitVector.BitVector2
-- Copyright   :  (c) 2010 Philip Weaver
-- License     :  BSD3
--
-- Maintainer  :  philip.weaver@gmail.com
-- Stability   :  provisional
-- Portability :  Haskell 98
--
-- (Description)
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.BitVector.BitVector2 where

import Prelude hiding (replicate, not, (&&), (||), Eq, Ord, and, or,
                       (==), (/=), (>), (<), (>=), (<=), min, max, length)
import qualified Prelude
import Data.Bits hiding (setBit)
import qualified Data.Bits as Bits
import Data.Generics    ( Data, Typeable )

import Data.Bit
import Data.BitVector.Util

import Data.Boolean
import Data.Compare

----------------------------------------
{- TODO
 - support range instead of width
 - check for invalid inputs to some functions
 - hide internal representation (do not export it)
-}

{-
  abit | bbit | value
    0  |   0  |   0
    1  |   0  |   1
    1  |   1  |   X
    0  |   1  |   Z
-}

-- using Integer as the type for abits and bbits means
-- that BitVector widths are unbounded, yay!
type BV_Word = Integer

-- index of MSB, index of LSB, a-bits, b-bits.
data BitVector
  = BV !Int !BV_Word !BV_Word
  deriving (Prelude.Ord, Data, Typeable)

instance Show BitVector where
  show = showBitVector showBit

showBit :: Bit -> Char
showBit F = '0'
showBit T = '1'
showBit U = 'X'
showBit Z = 'Z'

showBitVector :: (Bit -> Char) -> BitVector -> String
showBitVector f bv@(BV w _ _)
  | w == 0    = [f F]
  | otherwise = [ f (bv ! i) | i <- [w-1, w-2..0] ]

----------------------------------------
-- internal utility functions

-- {-# INLINE convertToBit #-}
convertToBit :: (Bool, Bool) -> Bit
convertToBit (False, False) = F
convertToBit (True, False)  = T
convertToBit (True, True)   = U
convertToBit (False, True)  = Z

{-# INLINE isKnown #-}
isKnown :: BitVector -> Bool
isKnown (BV _ _ b) = b == 0

-- {-# INLINE setBit #-}
setBit :: BitVector -> Int -> Bit -> BitVector
setBit (BV w a b) i x
  = BV w (setBitA a i x) (setBitB b i x)

-- {-# INLINE setBitA #-}
setBitA :: BV_Word -> Int -> Bit -> BV_Word
setBitA a i x
  = if a_bit x then Bits.setBit a i else Bits.clearBit a i

-- {-# INLINE setBitB #-}
setBitB :: BV_Word -> Int -> Bit -> BV_Word
setBitB b i x
  = if b_bit x then Bits.setBit b i else Bits.clearBit b i

-- {-# INLINE a_bit #-}
a_bit :: Bit -> Bool
a_bit = flip elem [T, U]
-- a_bit b = b == T || b == U
-- a_bit = flip testBit 0 . fromEnum

-- {-# INLINE b_bit #-}
b_bit :: Bit -> Bool
b_bit = flip elem [Z, U]
-- b_bit b = b == Z || b == U
-- b_bit = flip testBit 1 . fromEnum

----------------------------------------
-- initialization

empty :: BitVector
empty = BV 0 0 0

singleton :: Bit -> BitVector
singleton F = BV 1 0 0
singleton T = BV 1 1 0
singleton U = BV 1 1 1
singleton Z = BV 1 0 1

replicate :: Int -> Bit -> BitVector
replicate n F = BV n 0 0
replicate n T = BV n (2^n-1) 0
replicate n U = BV n (2^n-1) (2^n-1)
replicate n Z = BV n 0 (2^n-1)

low, high, unknown :: Int -> BitVector
low     = flip replicate F
high    = flip replicate T
unknown = flip replicate U

----------------------------------------
-- conversion

fromBits, fromBitsBE, fromBitsLE :: [Bit] -> BitVector
fromBits xs = BV w a'' b''
  where
    (a'', b'')     = f (w - 1) 0 0 xs
    w              = Prelude.length xs

    f _ a b []     = (a, b)
    f i a b (y:ys) = let a' = setBitA (a*2) 0 y
                         b' = setBitB (b*2) 0 y
                     in a' `seq` b' `seq` f (i-1) a' b' ys

fromBitsBE = fromBits
fromBitsLE = fromBits . Prelude.reverse

toBits, toBitsBE, toBitsLE :: BitVector -> [Bit]
toBits     = toBitsBE
toBitsBE v = map (indexR v) [w-1, w-2..0]
  where w = length v
toBitsLE v = map (indexR v) [0..w-1]
  where w = length v

toNum :: (Num a) => BitVector -> Maybe a
toNum (BV _ a b)
  = if b == 0
    then Just (fromIntegral a) -- (maskWidth w a)
    else Nothing

-- maskWidth makes this pretty slow
fromNum :: (Integral a) => Int -> a -> BitVector
fromNum w x
  -- = BV w (fromIntegral x) 0
  | w <= 0    = BV 0 0 0
  | otherwise = BV w (maskWidth w (fromIntegral x)) 0

----------------------------------------
-- length information

{-# INLINE length #-}
length :: BitVector -> Int
length (BV w _ _) = w

{-# INLINE null #-}
null :: BitVector -> Bool
null (BV w _ _)  = w == 0

----------------------------------------
-- map and fold functions

-- these have pretty terrible performance,
-- and should be avoded whenever possible.
-- for example, bv_and is ridiculously faster than bv_map2 (&&)

{-
bv_map :: (Bit -> Bit) -> BitVector -> BitVector
bv_map f v
  = BV (length v) a b
  where
    w          = length v
    (a, b)     = foldl' g (0, 0) [0..w-1]
    g (a, b) i = let x  = f (indexR v i)
                     a' = if a_bit x then Bits.setBit a i else a
                     b' = if b_bit x then Bits.setBit b i else b
                 in (a', b')

bv_map2 :: (Bit -> Bit -> Bit) -> BitVector -> BitVector -> BitVector
bv_map2 f v0 v1
  = BV w a b
  where
    w          = max (length v0) (length v1)
    (a, b)     = foldl' g (0, 0) [0..w-1]
    g (a, b) i = let x  = f (indexR v0 i) (indexR v1 i)
                     a' = if a_bit x then Bits.setBit a i else a
                     b' = if b_bit x then Bits.setBit b i else b
                 in (a', b')

bv_foldl :: (a -> Bit -> a) -> a -> BitVector -> a
bv_foldl f x v
  = g x 0
  where
    w     = length v
    g y i | i == w    = y
          | otherwise = g (f y (indexR v i)) (i+1)
-}

----------------------------------------
-- structural operations.
-- index, resizing, concatenation, shift, rotate

resize :: Int -> BitVector -> BitVector
resize n (BV w a b)
  | n < w
  = BV n (maskWidth n a) (maskWidth n b)
  | otherwise
  -- = BV n (maskWidth w a) (maskWidth w b)
  = BV n a b
  -- = BV n (maskWidth i a) (maskWidth i b)
  -- where
  --   i = min n w

-- sign-extend out to 'n' bits.
-- note that this will fail if n < w
signExtend :: Int -> BitVector -> BitVector
signExtend n (BV w a b)
  = BV n (signExtend' w n a) (signExtend' w n b)

signExtend' :: Int -> Int -> BV_Word -> BV_Word
signExtend' n n' x
  = if testBit x (n-1)
    then x .|. Bits.shiftL (2^(n'-n)-1) n
    else x

reverse :: BitVector -> BitVector
reverse (BV w a b)  = BV w a' b'
  where
    a' = f (w-1) a 0
    b' = f (w-1) b 0

    f 0 x y = if testBit x (w-1) then Bits.setBit y 0 else y
    f n x y = let y' = if testBit x (w-n-1) then Bits.setBit y n else y
              in y' `seq` f (n-1) x y'

(++), append :: BitVector -> BitVector -> BitVector
(++) = append
append (BV w0 a0 b0) (BV w1 a1 b1)
  = BV (w0 + w1) (Bits.shiftL a0 w1 .|. a1) (Bits.shiftL b0 w1 .|. b1)

concat :: [BitVector] -> BitVector
concat = foldr append empty

(!), indexR, indexL :: BitVector -> Int -> Bit
(!) = indexR

indexR (BV _ a b) i = convertToBit (testBit a i, testBit b i)

indexL v i = indexR v (length v - i - 1)

takeL :: Int -> BitVector -> BitVector
takeL i (BV w a b)
  = BV i (Bits.shiftR a (w-i)) (Bits.shiftR b (w-i))

takeR :: Int -> BitVector -> BitVector
takeR i (BV _ a b)
  = BV i (maskWidth i a) (maskWidth i b)

dropL :: Int -> BitVector -> BitVector
dropL i (BV w a b)
  = BV (w-i) (maskWidth (w-i) a) (maskWidth (w-i) b)

dropR :: Int -> BitVector -> BitVector
dropR i (BV w a b)
  = BV (w-i) (Bits.shiftR a i) (Bits.shiftR b i)

-- start at index 'start' and count down for 'len' total bits.
slice :: Int -> Int -> BitVector -> BitVector
slice start len (BV _ a b)
  = BV len (Bits.shiftR a (start - len + 1))
           (Bits.shiftR b (start - len + 1))

shiftL :: BitVector -> Int -> BitVector
shiftL (BV w a b) i
  = BV w (maskWidth w a') (maskWidth w b')
  where
    a' = Bits.shiftL a i
    b' = Bits.shiftL b i

shiftR :: BitVector -> Int -> BitVector
shiftR (BV w a b) i
  = BV w a' b'
  where
    a' = Bits.shiftR a i
    b' = Bits.shiftR b i

-- we can't use Bits.rotate to implement rotate because we use unbounded
-- Integer to represent the bitvector, so rotate just behaves like a shift
rotateL :: BitVector -> Int -> BitVector
rotateL (BV w a b) i0
  = BV w (maskWidth w a') (maskWidth w b')
  where
    i  = i0 `mod` w
    a' = (a `Bits.shiftR` (w-i)) .|. (a `Bits.shiftL` i)
    b' = (b `Bits.shiftR` (w-i)) .|. (b `Bits.shiftL` i)

rotateR :: BitVector -> Int -> BitVector
rotateR (BV w a b) i0
  = BV w (maskWidth w a') (maskWidth w b')
  where
    i  = i0 `mod` w
    a' = (a `Bits.shiftR` i) .|. (a `Bits.shiftL` (w-i))
    b' = (b `Bits.shiftR` i) .|. (b `Bits.shiftL` (w-i))

----------------------------------------
-- bitwise boolean operations, both binary and unary

-- these functions can all be defined in terms of bv_map, bv_map2 or bv_fold,
-- but the specialized versions below are ridiculously faster.

-- some functions perform operations (like complement) that can cause
-- bits beyond the MSB to be set.  in those cases, we must call maskWidth
-- to set those bits back to 0.

-- it would be cool if we had a function written in Template Haskell
-- that took a truth table and synthesized a function that used Data.Bits,
-- and then use that to define all of the functions below.

instance Boolean BitVector where
  false   = singleton false
  true    = singleton true
  isTrue  = isTrue  . flip indexR 0
  isFalse = isFalse . flip indexR 0
  not     = bv_not
  (&&)    = bv_and
  (||)    = bv_or
  xor     = bv_xor
  nor     = bv_nor
  xnor    = bv_xnor
  nand    = bv_nand

bv_not :: BitVector -> BitVector
bv_not (BV w a b)
  = BV w (maskWidth w a') b
  where
    a' = complement a .|. b

bv_and :: BitVector -> BitVector -> BitVector
bv_and (BV w0 a0 b0) (BV w1 a1 b1)
  = BV w a b
  where
    w = max w0 w1
    a = (a0 .|. b0) .&. (a1 .|. b1)
    -- b = (b0 .&. (a1 .|. b1)) .|. (b1 .&. (a0 .|. b0))
    b = (b0 .|. b1) .&. (a0 .|. b0) .&. (a1 .|. b1)

bv_or :: BitVector -> BitVector -> BitVector
bv_or (BV w0 a0 b0) (BV w1 a1 b1)
  = BV w a (maskWidth w b)
  where
    w = max w0 w1
    a = a0 .|. b0 .|. a1 .|. b1
    b = (b0 .&. (complement a1 .|. b1)) .|. (b1 .&. (complement a0 .|. b0))

bv_nor :: BitVector -> BitVector -> BitVector
bv_nor (BV w0 a0 b0) (BV w1 a1 b1)
  = BV w (maskWidth w a) (maskWidth w b)
  where
    w = max w0 w1
    a = b .|. (complement a0 .&. complement a1 .&.
               complement b0 .&. complement b1)
    b = (b0 .&. (complement a1 .|. b1)) .|.
        (b1 .&. (complement a0 .|. b0))

bv_xor :: BitVector -> BitVector -> BitVector
bv_xor (BV w0 a0 b0) (BV w1 a1 b1)
  = BV w a b
  where
    w = max w0 w1
    a = (a0 `Bits.xor` a1) .|. b0 .|. b1
    b = b0 .|. b1

bv_xnor :: BitVector -> BitVector -> BitVector
bv_xnor (BV w0 a0 b0) (BV w1 a1 b1)
  = BV w (maskWidth w a) b
  where
    w = max w0 w1
    a = (complement (a0 `Bits.xor` a1)) .|. b0 .|. b1
    b = b0 .|. b1

bv_nand :: BitVector -> BitVector -> BitVector
bv_nand (BV w0 a0 b0) (BV w1 a1 b1)
  = BV w (maskWidth w a) b
  where
    w = max w0 w1
    a = complement a0 .|. complement a1 .|. b0 .|. b1
    b = (b0 .&. (a1 .|. b1)) .|. (b1 .&. (a0 .|. b0))

bv_unary_and :: BitVector -> Bit
bv_unary_and (BV w a b)
  = if b == 0 then fromBool (a == 2^w-1)
    else if (maskWidth w (complement a .&. complement b)) /= 0 then F
    else U

bv_unary_or :: BitVector -> Bit
bv_unary_or (BV w a b)
  = if b == 0 then fromBool (a /= 0)
    else if (maskWidth w (a .&. complement b)) /= 0 then T
    else U

bv_unary_nor :: BitVector -> Bit
bv_unary_nor (BV w a b)
  = if b == 0 then fromBool (a == 0)
    else if (maskWidth w (a .&. complement b)) /= 0 then F
    else U

-- this implementation isn't very efficient, but it's not as bad
-- as just folding xor over 'a'.
bv_unary_xor :: BitVector -> Bit
bv_unary_xor (BV _ a b)
  = if b == 0 then fromBool (f a False) else U
  where
    f 0 r = r
    f n r = f (n .&. (n - 1)) (not r)

-- if all bits are known and all T or all F, then T.
-- if there are more than one known bit that don't match, then F.
-- FIXME this implementation is inefficient when any bits are unknown
bv_unary_xnor :: BitVector -> Bit
bv_unary_xnor v@(BV w a b)
  | b == 0    = fromBool (a == 0 || a == 2^w-1)
  | otherwise = and xs || not (or xs)
  where
    xs = toBits v

bv_unary_nand :: BitVector -> Bit
bv_unary_nand (BV w a b)
  = if b == 0 then fromBool (a /= 2^w-1)
    else if (maskWidth w (complement a .&. complement b)) /= 0 then T
    else U

----------------------------------------
-- comparision operations

instance Eq BitVector Bit where
  (==) = bv_eq
  (/=) = bv_neq

instance Eq BitVector BitVector where
  (==) x y = singleton (bv_eq x y)
  (/=) x y = singleton (bv_neq x y)

instance Prelude.Eq BitVector where
  (==) x y = bv_eq_xz x y
  (/=) x y = not (bv_eq_xz x y)

instance Compare BitVector where
  min = error "TODO min"
  max = error "TODO max"

instance Ord BitVector Bit where
  (>)  = bv_gt
  (<)  = bv_lt
  (>=) = bv_gte
  (<=) = bv_lte

instance Ord BitVector BitVector where
  (>)  x y = singleton (bv_gt x y)
  (<)  x y = singleton (bv_lt x y)
  (>=) x y = singleton (bv_gte x y)
  (<=) x y = singleton (bv_lte x y)

bv_eq :: BitVector -> BitVector -> Bit
bv_eq (BV w0 a0 b0) (BV w1 a1 b1)
  -- if all bits are known, then a0 == a1
  -- else if there are any known bits that are unequal, then False
  -- else Unknown.
  = if b0 == 0 && b1 == 0 then fromBool (a0 == a1)
    else if (maskWidth w (complement b0 .&. complement b1 .&. (Bits.xor a0 a1))) /= 0
         then F
    else U
  where
    w = max w0 w1

bv_neq :: BitVector -> BitVector -> Bit
bv_neq (BV w0 a0 b0) (BV w1 a1 b1)
  -- if all bits are known, then a0 /= a1
  -- else if there are any known bits that are not equal, then T
  -- else Unknown.
  = if b0 == 0 && b1 == 0 then fromBool (a0 /= a1)
    else if (maskWidth w (complement b0 .&. complement b1 .&. (Bits.xor a0 a1))) /= 0
         then T
    else U
  where
    w = max w0 w1

bv_eq_xz :: BitVector -> BitVector -> Bool
bv_eq_xz (BV _w0 a0 b0) (BV _w1 a1 b1)
  = a0 == a1 && b0 == b1

bv_lt :: BitVector -> BitVector -> Bit
bv_lt v0@(BV w0 a0 b0) v1@(BV w1 a1 b1)
  = if b0 == 0 && b1 == 0 then fromBool (a0 < a1)
    else f (w-1)
  where
    w = max w0 w1

    f (-1) = F
    f i = case (v0 ! i, v1 ! i) of
            (F, T) -> T
            (T, F) -> F
            (T, T) -> f (i-1)
            (F, F) -> f (i-1)
            _      -> U

bv_lte :: BitVector -> BitVector -> Bit
bv_lte v0@(BV w0 a0 b0) v1@(BV w1 a1 b1)
  = if b0 == 0 && b1 == 0 then fromBool (a0 <= a1)
    else f (w-1)
  where
    w = max w0 w1

    f (-1) = T
    f i = case (v0 ! i, v1 ! i) of
            (F, T) -> T
            (T, F) -> F
            (T, T) -> f (i-1)
            (F, F) -> f (i-1)
            _      -> U

bv_gt :: BitVector -> BitVector -> Bit
bv_gt v0@(BV w0 a0 b0) v1@(BV w1 a1 b1)
  = if b0 == 0 && b1 == 0 then fromBool (a0 > a1)
    else f (w-1)
  where
    w = max w0 w1

    f (-1) = F
    f i = case (v0 ! i, v1 ! i) of
            (T, F) -> T
            (F, T) -> F
            (T, T) -> f (i-1)
            (F, F) -> f (i-1)
            _      -> U

bv_gte :: BitVector -> BitVector -> Bit
bv_gte v0@(BV w0 a0 b0) v1@(BV w1 a1 b1)
  = if b0 == 0 && b1 == 0 then fromBool (a0 >= a1)
    else f (w-1)
  where
    w = max w0 w1

    f (-1) = T
    f i = case (v0 ! i, v1 ! i) of
            (T, F) -> T
            (F, T) -> F
            (T, T) -> f (i-1)
            (F, F) -> f (i-1)
            _      -> U

----------------------------------------
-- arithmetic functions

-- we have three main choices for handling unknowns in arithmetic operations:
--    * yield all unknown (this is what we do now)
--    * perform the operation on the lower known bits,
--      and yield unknown for the upper bits (iverilog does this, so do we)
--    * manually fold a bitwise function across the vector

{-# INLINE arithOp #-}
arithOp :: (BV_Word -> BV_Word -> BV_Word)
        -> Int -> Bool -> BitVector -> BitVector -> BitVector
arithOp f w signed v0 v1 = BV w a b
  where
    n          = min (msbIndex w b0) (msbIndex w b1)
    b          = Bits.shiftL (2^(w-n)-1) n
    a          = f a0 a1 .&. (2^w-1) .|. b
    BV _ a0 b0 = if signed then signExtend w v0 else v0
    BV _ a1 b1 = if signed then signExtend w v1 else v1

instance Num BitVector where
  (+) = plus' True
  (*) = times' True
  (-) = minus' True

  -- abs -- TODO

  signum v = replicate (length v) $ noZ $ indexL v 0

  negate v = bv_not v + 1

  fromInteger n
    | n < 0     = negate (fromInteger (-n))
    | otherwise = BV (neededBits n + 1) n 0

neg :: Int -> BitVector -> BitVector
neg w v
  = plus w True (bv_not v) 1

plus' :: Bool -> BitVector -> BitVector -> BitVector
plus' signed v0 v1
  = plus w signed v0 v1
  where
    w = 1 + max (length v0) (length v1)

plus :: Int -> Bool -> BitVector -> BitVector -> BitVector
plus = arithOp (+)

minus' :: Bool -> BitVector -> BitVector -> BitVector
minus' signed v0 v1
  = minus w signed v0 v1
  where
    w = 1 + max (length v0) (length v1)

minus :: Int -> Bool -> BitVector -> BitVector -> BitVector
minus = arithOp (-)

times' :: Bool -> BitVector -> BitVector -> BitVector
times' signed v0 v1
  = times w signed v0 v1
  where
    w = length v0 + length v1

times :: Int -> Bool -> BitVector -> BitVector -> BitVector
times = arithOp (*)

----------------------------------------
