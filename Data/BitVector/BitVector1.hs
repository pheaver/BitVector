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

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.BitVector.BitVector1 where

import Prelude hiding (replicate, not, (&&), (||), Eq, Ord, and, or,
                       (==), (/=), (>), (<), (>=), (<=), min, max, length)
import qualified Prelude
import Data.Generics    ( Data, Typeable )

import Data.Bit
import Data.BitVector.Util
import qualified Data.BitVector.UtilBE as BE
import qualified Data.BitVector.UtilLE as LE
import Data.Boolean as Boolean
import Data.Compare
import Data.Endianness

----------------------------------------

-- changing this variable will not affect the functionality of this module in
-- any way (assuming we've defined it correctly); it only affects the internal
-- representation, not the interface to the outside world.  changing this could
-- have a slight impact on performance
{-# INLINE internal_endianness #-}
internal_endianness :: Endianness
--internal_endianness = LittleEndian
internal_endianness = BigEndian

----------------------------------------

-- the internal representation of a BitVector
newtype BitVector
  = Bits [Bit]
  deriving (Prelude.Ord, Data, Typeable)

-- TODO
-- pretty print - take in base, yield [Doc] so user can insert punctuation

showBitVector :: Endianness -> (Bit -> Char) -> BitVector -> String
showBitVector e f (Bits bs) = map f (maybeReverse e bs)

instance Show BitVector where
  show = showBitVector BigEndian f
    where f T = '1'
          f F = '0'
          f U = 'X'
          f Z = 'Z'

----------------------------------------
-- initialization

empty :: BitVector
empty = Bits []

singleton :: Bit -> BitVector
singleton b = Bits [b]

replicate :: Int -> Bit -> BitVector
replicate n b = Bits (Prelude.replicate n b)

low, high, unknown :: Int -> BitVector
low     = flip replicate F
high    = flip replicate T
unknown = flip replicate U

----------------------------------------
-- conversion functions

{-# INLINE fromBits #-}
fromBits, fromBitsBE, fromBitsLE :: [Bit] -> BitVector
fromBits   = Bits
fromBitsBE = Bits . maybeReverse BigEndian
fromBitsLE = Bits . maybeReverse LittleEndian

{-# INLINE toBits #-}
toBits, toBitsBE, toBitsLE :: BitVector -> [Bit]
toBits   (Bits bs) = bs
toBitsBE (Bits bs) = maybeReverse BigEndian bs
toBitsLE (Bits bs) = maybeReverse LittleEndian bs

maybeReverse :: Endianness -> [a] -> [a]
maybeReverse e xs
  | e == internal_endianness = xs
  | otherwise                = reverse xs

toNum :: Num a => BitVector -> Maybe a
toNum (Bits v)
  = case mapM toBool v of
      Just bs -> let n = Prelude.length v
                     f i b = if b then 2^(n-1-i) else 0
                 in Just $ sum $ zipWith f [0..n-1] bs
      Nothing -> Nothing

fromNum :: (Integral a) => Int -> a -> BitVector
fromNum w x
  = case internal_endianness of
      BigEndian    -> Bits (BE.numToBits w (Prelude.toInteger x))
      LittleEndian -> Bits (LE.numToBits w (Prelude.toInteger x))

----------------------------------------
-- length information

{-# INLINE length #-}
length :: BitVector -> Int
length (Bits v)   = Prelude.length v

{-# INLINE null #-}
null :: BitVector -> Bool
null (Bits v)     = Prelude.null v

----------------------------------------
-- structural operations
-- index, resizing, concatenation, shift, rotate

-- pad: add 'n' 0s on the MSB end
pad :: Int -> BitVector -> BitVector
pad n (Bits bs)
  = Bits $ case internal_endianness of
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

-- sign-extend out to 'n' bits.
-- note: for n <= w, this does nothing
signExtend :: Int -> BitVector -> BitVector
signExtend n (Bits xs)
  = Bits (Prelude.replicate (n-w) msb Prelude.++ xs)
  where
    w = Prelude.length xs

    msb = case xs of
            []    -> F
            (x:_) -> noZ x

-- cons :: Bit -> BitVector -> BitVector
-- snoc :: BitVector -> Bit -> BitVector

(++), append :: BitVector -> BitVector -> BitVector
(++) = append
append (Bits v1) (Bits v2)
  = Bits $ case internal_endianness of
            BigEndian    -> v1 Prelude.++ v2
            LittleEndian -> v2 Prelude.++ v1

concat :: [BitVector] -> BitVector
concat = foldr append empty

(!), indexR, indexL :: BitVector -> Int -> Bit
(!) = indexR

indexR (Bits v) i
  = case internal_endianness of
      LittleEndian -> v !! i
      BigEndian    -> v !! (Prelude.length v - i -1)

indexL (Bits v) i
  = case internal_endianness of
      LittleEndian -> v !! (Prelude.length v - i -1)
      BigEndian    -> v !! i


-- TODO throw error if out of range?
takeL, takeR, dropL, dropR :: Int -> BitVector -> BitVector
takeL i (Bits bs)
  = Bits $ case internal_endianness of
            LittleEndian -> drop (Prelude.length bs - i) bs
            BigEndian    -> take i bs

takeR i (Bits bs)
  = Bits $ case internal_endianness of
            LittleEndian -> take i bs
            BigEndian    -> drop (Prelude.length bs - i) bs

dropL i (Bits bs)
  = Bits $ case internal_endianness of
            LittleEndian -> take (Prelude.length bs - i) bs
            BigEndian    -> drop i bs

dropR i (Bits bs)
  = Bits $ case internal_endianness of
            LittleEndian -> drop i bs
            BigEndian    -> take (Prelude.length bs - i) bs

{-
sliceR, sliceL :: Int -> Int -> BitVector -> BitVector

-- [i-:n]
sliceR i n = takeL n . takeR (i+1)

-- [i+:n]
sliceL i n = takeL n . dropL i
-}

slice :: Int -> Int -> BitVector -> BitVector
slice start len (Bits xs)
  = Bits (take len $ drop (Prelude.length xs - start - 1) xs)

shiftL :: BitVector -> Int -> BitVector
shiftL (Bits xs) i
  = Bits (drop i xs Prelude.++ Prelude.replicate (min i w) F)
  where
    w = Prelude.length xs

shiftR :: BitVector -> Int -> BitVector
shiftR (Bits xs) i
  = Bits (Prelude.replicate (min i w) F Prelude.++ take (w-i) xs)
  where
    w = Prelude.length xs

-- we can't use Bits.rotate to implement rotate because we use unbounded
-- Integer to represent the bitvector, so rotate just behaves like a shift
rotateL :: BitVector -> Int -> BitVector
rotateL (Bits xs) i0
  = Bits (drop i xs Prelude.++ take i xs)
  where
    w = Prelude.length xs
    i = i0 `mod` w

rotateR :: BitVector -> Int -> BitVector
rotateR (Bits xs) i0
  = Bits (drop (w-i) xs Prelude.++ take (w-i) xs)
  where
    w = Prelude.length xs
    i = i0 `mod` w

----------------------------------------
-- bitwise boolean operations, both binary and unary

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
bv_not (Bits xs)
  = Bits (map not xs)

bv_and :: BitVector -> BitVector -> BitVector
bv_and (Bits xs0) (Bits xs1)
  = Bits (zipWith (&&) xs0' xs1')
  where
    w0   = Prelude.length xs0
    w1   = Prelude.length xs1
    w    = max w0 w1
    xs0' = Prelude.replicate (w - w0) F Prelude.++ xs0
    xs1' = Prelude.replicate (w - w1) F Prelude.++ xs1

bv_or :: BitVector -> BitVector -> BitVector
bv_or (Bits xs0) (Bits xs1)
  = Bits (zipWith (||) xs0' xs1')
  where
    w0   = Prelude.length xs0
    w1   = Prelude.length xs1
    w    = max w0 w1
    xs0' = Prelude.replicate (w - w0) F Prelude.++ xs0
    xs1' = Prelude.replicate (w - w1) F Prelude.++ xs1

bv_nor :: BitVector -> BitVector -> BitVector
bv_nor (Bits xs0) (Bits xs1)
  = Bits (zipWith nor xs0' xs1')
  where
    w0   = Prelude.length xs0
    w1   = Prelude.length xs1
    w    = max w0 w1
    xs0' = Prelude.replicate (w - w0) F Prelude.++ xs0
    xs1' = Prelude.replicate (w - w1) F Prelude.++ xs1

bv_xor :: BitVector -> BitVector -> BitVector
bv_xor (Bits xs0) (Bits xs1)
  = Bits (zipWith Boolean.xor xs0' xs1')
  where
    w0   = Prelude.length xs0
    w1   = Prelude.length xs1
    w    = max w0 w1
    xs0' = Prelude.replicate (w - w0) F Prelude.++ xs0
    xs1' = Prelude.replicate (w - w1) F Prelude.++ xs1

bv_xnor :: BitVector -> BitVector -> BitVector
bv_xnor (Bits xs0) (Bits xs1)
  = Bits (zipWith xnor xs0' xs1')
  where
    w0   = Prelude.length xs0
    w1   = Prelude.length xs1
    w    = max w0 w1
    xs0' = Prelude.replicate (w - w0) F Prelude.++ xs0
    xs1' = Prelude.replicate (w - w1) F Prelude.++ xs1

bv_nand :: BitVector -> BitVector -> BitVector
bv_nand (Bits xs0) (Bits xs1)
  = Bits (zipWith nand xs0' xs1')
  where
    w0   = Prelude.length xs0
    w1   = Prelude.length xs1
    w    = max w0 w1
    xs0' = Prelude.replicate (w - w0) F Prelude.++ xs0
    xs1' = Prelude.replicate (w - w1) F Prelude.++ xs1

bv_unary_and :: BitVector -> Bit
bv_unary_and (Bits xs)
  = foldl (&&) T xs

bv_unary_or :: BitVector -> Bit
bv_unary_or (Bits xs)
  = foldl (||) F xs

bv_unary_nor :: BitVector -> Bit
bv_unary_nor (Bits []) = T
bv_unary_nor (Bits (x:xs))
  = foldl nor x xs

bv_unary_xor :: BitVector -> Bit
bv_unary_xor (Bits xs)
  = foldl Boolean.xor F xs

-- unary_xnor means "all true or all false"
-- if all bits are known and all T or all F, then T.
-- if there are more than one known bit that don't match, then F.
-- FIXME this implementation is inefficient when any bits are unknown
bv_unary_xnor :: BitVector -> Bit
bv_unary_xnor (Bits xs)
  = and xs || not (or xs)

bv_unary_nand :: BitVector -> Bit
bv_unary_nand (Bits xs)
  = not (foldl (&&) T xs)

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
bv_eq (Bits xs0) (Bits xs1)
  = and (zipWith (==) xs0' xs1')
  where
    w0   = Prelude.length xs0
    w1   = Prelude.length xs1
    w    = max w0 w1
    xs0' = Prelude.replicate (w - w0) F Prelude.++ xs0
    xs1' = Prelude.replicate (w - w1) F Prelude.++ xs1

bv_neq :: BitVector -> BitVector -> Bit
bv_neq (Bits xs0) (Bits xs1)
  = or (zipWith (/=) xs0' xs1')
  where
    w0   = Prelude.length xs0
    w1   = Prelude.length xs1
    w    = max w0 w1
    xs0' = Prelude.replicate (w - w0) F Prelude.++ xs0
    xs1' = Prelude.replicate (w - w1) F Prelude.++ xs1

bv_eq_xz :: BitVector -> BitVector -> Bool
bv_eq_xz (Bits xs0) (Bits xs1)
  = xs0' == xs1'
  where
    w0   = Prelude.length xs0
    w1   = Prelude.length xs1
    w    = max w0 w1
    xs0' = Prelude.replicate (w - w0) F Prelude.++ xs0
    xs1' = Prelude.replicate (w - w1) F Prelude.++ xs1

bv_lt :: BitVector -> BitVector -> Bit
bv_lt v0 v1
  = f xs0 xs1
  where
    w = max (length v0) (length v1)

    xs0 = toBits (resize w v0)
    xs1 = toBits (resize w v1)

    f [] []         = F
    f (y:ys) (z:zs) = case (y, z) of
                        (F, T) -> T
                        (T, F) -> F
                        (T, T) -> f ys zs
                        (F, F) -> f ys zs
                        _      -> U
    f _ _ = error "bv_lt"

bv_lte :: BitVector -> BitVector -> Bit
bv_lte v0 v1
  = f xs0 xs1
  where
    w = max (length v0) (length v1)

    xs0 = toBits (resize w v0)
    xs1 = toBits (resize w v1)

    f [] []         = T
    f (y:ys) (z:zs) = case (y, z) of
                        (F, T) -> T
                        (T, F) -> F
                        (T, T) -> f ys zs
                        (F, F) -> f ys zs
                        _      -> U
    f _ _ = error "bv_lte"

bv_gt :: BitVector -> BitVector -> Bit
bv_gt v0 v1
  = f xs0 xs1
  where
    w = max (length v0) (length v1)

    xs0 = toBits (resize w v0)
    xs1 = toBits (resize w v1)

    f [] []         = F
    f (y:ys) (z:zs) = case (y, z) of
                        (T, F) -> T
                        (F, T) -> F
                        (T, T) -> f ys zs
                        (F, F) -> f ys zs
                        _      -> U
    f _ _ = error "bv_gt"


bv_gte :: BitVector -> BitVector -> Bit
bv_gte v0 v1
  = f xs0 xs1
  where
    w = max (length v0) (length v1)

    xs0 = toBits (resize w v0)
    xs1 = toBits (resize w v1)

    f [] []         = T
    f (y:ys) (z:zs) = case (y, z) of
                        (T, F) -> T
                        (F, T) -> F
                        (T, T) -> f ys zs
                        (F, F) -> f ys zs
                        _      -> U
    f _ _ = error "bv_gte"


-- --------------------

{-# INLINE arithOp #-}
arithOp :: (Integer -> Integer -> Integer)
        -> Int -> Bool -> BitVector -> BitVector -> BitVector
arithOp f n signed x y
  = case (toNum x', toNum y') of
      (Just v1, Just v2) -> fromNum n (f v1 v2)
      _                  -> Bits (Prelude.replicate n U)
  where
    x' = if signed then signExtend n x else x
    y' = if signed then signExtend n y else y

instance Num BitVector where
  (+) = plus' True
  (*) = times' True
  (-) = minus' True

  -- abs -- TODO

  signum v = replicate (length v) $ noZ $ indexL v 0

  negate v = bv_not v + 1

  fromInteger n
    | n < 0     = negate (fromInteger (-n))
    | otherwise = fromNum (neededBits n + 1) n

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

-- handle case where argument is partially defined, by yielding a partially
-- defined result
{-
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
-}

-- --------------------
