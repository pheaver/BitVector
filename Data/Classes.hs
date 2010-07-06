-- for lack of a better place, all these classes are lumped into this one file

{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE OverlappingInstances    #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE FunctionalDependencies  #-}

module Data.Classes where

import Prelude hiding (Ord(..), Eq(..), (&&), (||), not)
import qualified Prelude
import Control.Monad     ( liftM, liftM2 )

-- --------------------

-- not sure about these precedences, just copied them from GHC's definitions
infixr 3 &&
infixr 2 ||
infix 4 ==, /=
infix 4 <, >, <=, >=

infixl 6 `xor`
infixl 6 `xnor`

class SubType a b where
  inj :: a -> b
  prj :: b -> Maybe a

class Boolean a where
  true    :: a
  false   :: a
  isTrue  :: a -> Bool
  isFalse :: a -> Bool

  (&&), (||), xor, xnor :: a -> a -> a
  not :: a -> a

class Eq a b where
  (==), (/=) :: a -> a -> b

-- this is a separate class from 'Ord' because it doesn't mention the type 'b'
class Compare a where
  min, max :: a -> a -> a

class Compare a => Ord a b where
  (>), (<), (>=), (<=) :: a -> a -> b

-- --------------------

instance Boolean Bool where
  true    = True
  false   = False
  isTrue  = id
  isFalse = Prelude.not
  (&&)    = (Prelude.&&)
  (||)    = (Prelude.||)
  xor     = (Prelude./=)
  xnor    = (Prelude.==)
  not     = Prelude.not

instance Boolean (Maybe Bool) where
  true       = Just True
  false      = Just False
  isTrue     = maybe False id
  isFalse    = maybe False (Prelude.not)
  (&&)       = liftM2 (&&)
  (||)       = liftM2 (||)
  xor        = liftM2 xor
  xnor       = liftM2 xnor
  not        = liftM not

instance Prelude.Eq a => Eq a Bool where
  (==) = (Prelude.==)
  (/=) = (Prelude./=)

instance Prelude.Ord a => Compare a where
  min  = Prelude.min
  max  = Prelude.max

instance Prelude.Ord a => Ord a Bool where
  (<)   = (Prelude.<)
  (<=)  = (Prelude.<=)
  (>)   = (Prelude.>)
  (>=)  = (Prelude.>=)

instance (SubType Bool a, Eq a a) => Eq a (Maybe Bool) where
  x == y  = prj (x == y :: a)
  x /= y  = prj (x == y :: a)

instance (SubType Bool a, Ord a a) => Ord a (Maybe Bool) where
  x <  y  = prj (x <  y :: a)
  x <= y  = prj (x <= y :: a)
  x >  y  = prj (x >  y :: a)
  x >= y  = prj (x >= y :: a)

eq, neq :: Eq a a => a -> a -> a
eq  = (==)
neq = (/=)

lt, lte, gt, gte :: Ord a a => a -> a -> a
lt  = (<)
lte = (<=)
gt  = (>)
gte = (>=)

-- --------------------
