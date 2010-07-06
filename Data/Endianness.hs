--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Endianness
-- Copyright   :  (c) 2010 Philip Weaver
-- License     :  BSD3
--
-- Maintainer  :  philip.weaver@gmail.com
-- Stability   :
-- Portability :
--
-- (Description)
--------------------------------------------------------------------------------

module Data.Endianness where

data Endianness = LittleEndian | BigEndian
                  deriving (Prelude.Eq)

{- Discussion of endianness

Due to the nuances of the languages, endianness can mean something different in
Verilog and Haskell.

The verilog definition of endianness is the standard, but we are prescribing the
Haskell definition.

  * In Verilog bit-vectors, endianness determines how we index into the vector.

  * In Haskell lists, endianness determines which side of the list the least
    significant bit is.

== Verilog ==

In Verilog, words are always written with the least significant on the right.
The word {0,0,1,0} corresponds to the number 2 regardless of endianness.

Endianness simply determines how we index into a word.  In little endian, index
0 is on the right, and in big endian, it is on the left.  Thus, in the
bit-vector {0,0,1,0}, the 1 is at index 1 if it is little endian and index 2 if
it is big endian.

The endianness of a bit-vector is determined by its declaration:

  wire [3:0] x;  // little endian
  wire [0:3] y;  // big endian

Index ranges must follow the same direction as the declaration.  Given the above
declarations, x[1:0] would be a valid slice, but x[0:1] would not.

== Haskell ==

In Haskell, lists are written left to right, with index 0 being on the left.  We
say that a list of bits is little endian if the least significant is first (at
index 0), and big endian if the most significant is first.  So, the list
[0,1,0,0] represents 2 if it is little endian and 4 if it is big endian, but the
1 is always at index 1, regardless of endianness.

This definition of endianness is different than the Verilog notion of
endianness.  In fact, Verilog bit-vectors are most like big-endian Haskell
lists, because all Verilog bit-vectors have the least significant bit on the
right.

-}
