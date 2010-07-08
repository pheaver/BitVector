{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp      #-}

module Data.Bit where

import Prelude hiding (Ord(..), Eq(..), (&&), (||), not, (==))
import qualified Prelude
import Data.Generics    ( Data, Typeable )

import Data.Boolean
import Data.Compare

-- --------------------
-- true(1), false(0), unknown(U)

data Bit
  = F | T | Z | U
  deriving (Prelude.Eq, Prelude.Ord, Show, Bounded, Enum, Data, Typeable)

instance Conditional Bit Bit where
  ifc T x _ = x
  ifc F _ y = y
  ifc _ x y = x == y

-- zzz: a bit of a hack.  what if the lists are different widths?
instance Conditional Bit [Bit] where
  ifc T xs _  = xs
  ifc F _ ys  = ys
  ifc _ xs ys = [ U | _ <- xs | _ <- ys ]

instance Conditional Bit ([Bit], [Bit]) where
  ifc T x _ = x
  ifc F _ y = y
  ifc _ ~(xs1, xs2) ~(ys1, ys2) = (as, bs)
    where
      as = [ U | _ <- xs1 | _ <- ys1 ]
      bs = [ U | _ <- xs2 | _ <- ys2 ]

{-# INLINE noZ #-}
noZ :: Bit -> Bit
noZ Z = U
noZ x = x

isKnown :: Bit -> Bool
isKnown T = True
isKnown F = True
isKnown _ = False

fromBool :: Bool -> Bit
fromBool True  = T
fromBool False = F

toBool :: Bit -> Maybe Bool
toBool T = Just True
toBool F = Just False
toBool _ = Nothing

instance Boolean Bit where
  true      = T
  false     = F
  isTrue T  = True
  isTrue _  = False
  isFalse F = True
  isFalse _ = False

  F && _    = F
  _ && F    = F
  T && T    = T
  _ && _    = U

  T || _    = T
  _ || T    = T
  F || F    = F
  _ || _    = U

  not T     = F
  not F     = T
  not _     = U

  nand T T  = F
  nand F _  = T
  nand _ F  = T
  nand _ _  = U

  xor T T   = F
  xor F F   = F
  xor T F   = T
  xor F T   = T
  xor _ _   = U

  nor F F   = T
  nor T _   = F
  nor _ T   = F
  nor _ _   = U

  xnor T T  = T
  xnor F F  = T
  xnor T F  = F
  xnor F T  = F
  xnor _ _  = U

instance Eq Bit Bit where
  (==) = xnor
  (/=) = xor

-- not 100% sure about this one
instance Compare Bit where
  min F _ = F
  min _ F = F
  min T T = T
  min _ _ = U

  max T _ = T
  max _ T = T
  max F F = F
  max _ _ = U

-- not 100% sure about this one
-- in Verilog simulation, (at least iverilog) 1 >= U (and U <= 1) is defined,
-- but 0 <= U (and U >= 0) is undefined
instance Ord Bit Bit where
  T < _   = F
  F < T   = T
  F < F   = F
  _ < _   = U

  F > _   = F
  T > F   = T
  T > T   = F
  _ > _   = U

  T >= _  = T
  _ >= F  = T
  F >= T  = F
  _ >= _  = U

  F <= _  = T
  _ <= T  = T
  T <= F  = F
  _ <= _  = U

-- --------------------
