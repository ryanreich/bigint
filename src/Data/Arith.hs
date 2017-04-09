{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Arith where

import GHC.Prim
import GHC.Types

-- | We need this faux-'Nat' kind so we can actually induct, which AFAIK
-- you can't actually do with a Nat.
data Peano = Zero | Succ Peano

-- | Here, @n@ is the number of machine-word-sized "digits" of the big
-- word.
data family BigWord (n :: Peano)
-- | Can't use a newtype because they don't take unboxed arguments
data instance BigWord Zero = MachineWord# Word#
-- | Here, the @UNPACK@ pragma should, hopefully, result recursively in the
-- 'BigWord' constructor containing, in its storage representation, a nice
-- ordered list of the digits of the big word, without any indirection and
-- with storage locality.  We can't use a newtype here because the
-- strictness annotation is required, but I think I can live with that.
data instance BigWord (Succ n) = 
  BigWord {-# UNPACK #-}!(BigWord n, BigWord n)

lowWord :: BigWord (Succ n) -> BigWord n
lowWord (BigWord (x, _)) = x

class BigArith (n :: Peano) where
  -- | Addition with carrying
  overAdd :: BigWord n -> BigWord n -> BigWord (Succ n)
  -- | Multiplication with carrying
  overMul :: BigWord n -> BigWord n -> BigWord (Succ n)

instance BigArith Zero where
  -- | This is built-in as 'plusWord2#'
  overAdd (MachineWord# x#) (MachineWord# y#) = 
    BigWord (MachineWord# z0#, MachineWord# z1#)
    where (# z1#, z0# #) = plusWord2# x# y#

  -- | This is built-in as 'timesWord2#'
  overMul (MachineWord# x#) (MachineWord# y#) = 
    BigWord (MachineWord# z0#, MachineWord# z1#)
    where (# z1#, z0# #) = timesWord2# x# y#

instance
  (
    BigArith n
  ) =>
  Num (BigWord n) where

  x + y = lowWord $ overAdd x y
  x * y = lowWord $ overMul x y

  (-) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

