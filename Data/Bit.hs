{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Bit where

import Prelude hiding (Ord(..), Eq(..), (&&), (||), not)
import qualified Prelude
import Data.Generics    ( Data, Typeable )

import Data.Classes

-- --------------------
-- true(1), false(0), unknown(U)

data Bit
  = F | T | U
  deriving (Prelude.Eq, Prelude.Ord, Show, Bounded, Enum, Data, Typeable)

instance SubType Bool Bool where
  inj = id
  prj = Just

instance SubType Bool Bit where
  inj True  = T
  inj False = F
  prj T     = Just True
  prj F     = Just False
  prj _     = Nothing

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

  xor T T   = F
  xor F F   = F
  xor T F   = T
  xor F T   = T
  xor _ _   = U

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
