----------------------------------------

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.BitVector where

import Prelude hiding (replicate, not, (&&), (||), Eq, Ord,
                       (==), (/=), (>), (<), (>=), (<=), min, max)
import qualified Prelude
import Data.Bits hiding (setBit)
import qualified Data.Bits as Bits
import Data.Generics    ( Data, Typeable )
import Data.List (foldl')
import Data.Maybe (mapMaybe)

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
  = BV { bv_width  :: !Int
       , bv_abits  :: !BV_Word
       , bv_bbits  :: !BV_Word
       }
  deriving (Prelude.Eq, Prelude.Ord, Data, Typeable)

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
  | otherwise = [ f (getBit bv i) | i <- [w-1, w-2..0] ]

----------------------------------------

-- {-# INLINE getWidth #-}
getWidth :: BitVector -> Int
getWidth = bv_width

-- {-# INLINE getBit #-}
getBit :: BitVector -> Int -> Bit
getBit (BV _ a b) = getBit' a b

-- {-# INLINE getBit' #-}
getBit' :: BV_Word -> BV_Word -> Int -> Bit
getBit' a b i
  = convertToBit (testBit a i, testBit b i)

-- {-# INLINE convertToBit #-}
convertToBit :: (Bool, Bool) -> Bit
convertToBit (False, False) = F
convertToBit (True, False)  = T
convertToBit (True, True)   = U
convertToBit (False, True)  = Z

{-# INLINE isKnown #-}
isKnown :: BitVector -> Bool
isKnown = (==0) . bv_bbits

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
-- functions to create and convert bit-vectors

low, high, unknown :: Int -> BitVector
low     = flip replicate F
high    = flip replicate T
unknown = flip replicate U

replicate :: Int -> Bit -> BitVector
replicate n F = BV n 0 0
replicate n T = BV n (2^n-1) 0
replicate n U = BV n (2^n-1) (2^n-1)
replicate n Z = BV n 0 (2^n-1)

toNum :: (Num a) => BitVector -> Maybe a
toNum (BV w a b)
  = if b == 0
    then Just (fromIntegral a) -- (maskWidth w a)
    else Nothing

-- maskWidth makes this pretty slow
fromNum :: (Integral a) => Int -> a -> BitVector
fromNum w x
  -- = BV w (fromIntegral x) 0
  | w <= 0    = BV 0 0 0
  | otherwise = BV w (maskWidth w (fromIntegral x)) 0

fromBits :: [Bit] -> BitVector
fromBits xs = BV w a b
  where
    (a, b)         = f (w - 1) 0 0 xs
    w              = length xs

    f _ a b []     = (a, b)
    f i a b (y:ys) = let a' = setBitA (a*2) 0 y
                         b' = setBitB (b*2) 0 y
                     in a' `seq` b' `seq` f (i-1) a' b' ys

toBits :: BitVector -> [Bit]
toBits v = map (getBit v) [w-1, w-2..0]
  where w = getWidth v

----------------------------------------
-- map and fold functions

-- these have pretty terrible performance,
-- and should be avoded whenever possible.
-- for example, bv_and is ridiculously faster than bv_map2 (&&)

bv_map :: (Bit -> Bit) -> BitVector -> BitVector
bv_map f v
  = BV (getWidth v) a b
  where
    w          = getWidth v
    (a, b)     = foldl' g (0, 0) [0..w-1]
    g (a, b) i = let x  = f (getBit v i)
                     a' = if a_bit x then Bits.setBit a i else a
                     b' = if b_bit x then Bits.setBit b i else b
                 in (a', b')

bv_map2 :: (Bit -> Bit -> Bit) -> BitVector -> BitVector -> BitVector
bv_map2 f v0 v1
  = BV w a b
  where
    w          = max (getWidth v0) (getWidth v1)
    (a, b)     = foldl' g (0, 0) [0..w-1]
    g (a, b) i = let x  = f (getBit v0 i) (getBit v1 i)
                     a' = if a_bit x then Bits.setBit a i else a
                     b' = if b_bit x then Bits.setBit b i else b
                 in (a', b')

bv_foldl :: (a -> Bit -> a) -> a -> BitVector -> a
bv_foldl f x v
  = g x 0
  where
    w     = getWidth v
    g y i | i == w    = y
          | otherwise = g (f y (getBit v i)) (i+1)

----------------------------------------
-- structural operations

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

(++), append :: BitVector -> BitVector -> BitVector
(++) = append
append (BV w0 a0 b0) (BV w1 a1 b1)
  = BV (w0 + w1) (Bits.shiftL a0 w1 .|. a1) (Bits.shiftL b0 w1 .|. b1)

concat :: [BitVector] -> BitVector
concat xs = foldl append (BV 0 0 0) xs

index, indexR, indexL :: BitVector -> Int -> Bit
index      = getBit
indexR     = getBit
indexL v i = getBit v (bv_width v - i - 1)

takeL :: Int -> BitVector -> BitVector
takeL i (BV w a b)
  = BV i (Bits.shiftR a (w-i)) (Bits.shiftR b (w-i))

takeR :: Int -> BitVector -> BitVector
takeR i (BV w a b)
  = BV i (maskWidth i a) (maskWidth i b)

dropL :: Int -> BitVector -> BitVector
dropL i (BV w a b)
  = BV (w-i) (maskWidth (w-i) a) (maskWidth (w-i) b)

dropR :: Int -> BitVector -> BitVector
dropR i (BV w a b)
  = BV (w-i) (Bits.shiftR a i) (Bits.shiftR b i)

-- start at index 'start' and count down for 'len' total bits.
slice :: Int -> Int -> BitVector -> BitVector
slice start len (BV w a b)
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
-- bitwise boolean operations, both binary (map/zip) and unary (fold)

-- these functions can all be defined in terms of bv_map, bv_map2 or bv_fold,
-- but the specialized versions below are ridiculously faster.

-- some functions perform operations (like complement) that can cause
-- bits beyond the MSB to be set.  in those cases, we must call maskWidth
-- to set those bits back to 0.

-- it would be cool if we had a function written in Template Haskell
-- that took a truth table and synthesized a function that used Data.Bits,
-- and then use that to define all of the functions below.

instance Boolean BitVector where
  true    = BV 1 1 0
  false   = BV 1 0 0
  isTrue  = isTrue  . flip getBit 0
  isFalse = isFalse . flip getBit 0
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

bv_fold_and :: BitVector -> Bit
bv_fold_and v@(BV w a b)
  -- = bv_foldl (&&) F v
  = if b == 0 then fromBool (a == 2^w-1)
    else if (maskWidth w (complement a .&. complement b)) /= 0 then F
    else U

bv_fold_or :: BitVector -> Bit
bv_fold_or v@(BV w a b)
  -- = bv_foldl (||) F v
  = if b == 0 then fromBool (a /= 0)
    else if (maskWidth w (a .&. complement b)) /= 0 then T
    else U

bv_fold_nor :: BitVector -> Bit
bv_fold_nor v@(BV w a b)
  = if b == 0 then fromBool (a == 0)
    else if (maskWidth w (a .&. complement b)) /= 0 then F
    else U

-- this implementation isn't very efficient, but it's not as bad
-- as just folding xor over 'a'.
bv_fold_xor :: BitVector -> Bit
bv_fold_xor v@(BV w a b)
  = if b == 0 then fromBool (f a False) else U
  where
    f 0 r = r
    f n r = f (n .&. (n - 1)) (not r)

-- if all bits are known and all T or all F, then T.
-- if there are more than one known bit that don't match, then F.
-- FIXME this implementation is inefficient when any bits are unknown
bv_fold_xnor :: BitVector -> Bit
bv_fold_xnor v@(BV w a b)
  = if b == 0 then fromBool (a == 0 || a == 2^w-1)
    else if or xs && not (and xs) then F
    else U
  where
    -- get all known bits
    xs :: [Bool]
    xs = mapMaybe toBool (toBits v)

bv_fold_nand :: BitVector -> Bit
bv_fold_nand v@(BV w a b)
  = if b == 0 then fromBool (a /= 2^w-1)
    else if (maskWidth w (complement a .&. complement b)) /= 0 then T
    else U

----------------------------------------
-- comparision operations

instance Eq BitVector Bit where
  (==) = bv_eq
  (/=) = bv_neq

instance Eq BitVector Bool where
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
    f i = case (getBit v0 i, getBit v1 i) of
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
    f i = case (getBit v0 i, getBit v1 i) of
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
    f i = case (getBit v0 i, getBit v1 i) of
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
    f i = case (getBit v0 i, getBit v1 i) of
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

  signum v = replicate (bv_width v) $
             case indexL v 0 of Z -> U
                                x -> x

  negate v = bv_not v + 1

  fromInteger n
    | n < 0     = negate (fromInteger (-n))
    | otherwise = BV (neededBits n + 1) n 0

plus' :: Bool -> BitVector -> BitVector -> BitVector
plus' signed v0 v1
  = plus w signed v0 v1
  where
    w = 1 + max (bv_width v0) (bv_width v1)

plus :: Int -> Bool -> BitVector -> BitVector -> BitVector
plus = arithOp (+)

minus' :: Bool -> BitVector -> BitVector -> BitVector
minus' signed v0 v1
  = minus w signed v0 v1
  where
    w = 1 + max (bv_width v0) (bv_width v1)

minus :: Int -> Bool -> BitVector -> BitVector -> BitVector
minus = arithOp (-)

times' :: Bool -> BitVector -> BitVector -> BitVector
times' signed v0 v1
  = times w signed v0 v1
  where
    w = bv_width v0 + bv_width v1

times :: Int -> Bool -> BitVector -> BitVector -> BitVector
times = arithOp (*)

----------------------------------------
