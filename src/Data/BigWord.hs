{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.BigWord
  (
    BigWord, BigWordStep
  ) where

import Data.Bits
import Data.Function
import Data.Monoid
import Data.Ord

import GHC.Prim
import GHC.TypeLits
import GHC.Types

-- The general interface

-- | @DoubleWord a ~ (a, a)@
data family DoubleWord a
class LowHigh a where
  -- | Must be a data and not a type so that we can define instances
  low :: DoubleWord a -> a
  high :: DoubleWord a -> a
  makeLowHigh :: a -> a -> DoubleWord a

-- | A subset of the `Num` operations
class Semiring a where
  zero :: a
  -- | This is actually a fancy version of the unit 1, since the
  -- characteristic function is just integers interpreted as multiples of
  -- 1.
  char :: Integer -> (a, Integer)

  add :: a -> a -> a
  mul :: a -> a -> a

-- | Arithmetic with carrying
class (Semiring a) => BigWordArith a where
  inject :: Word -> a
  overAdd :: a -> a -> (a, Word)
  overMul :: a -> a -> (a, a)

instance {-# OVERLAPPABLE #-} (Semiring a) => Num a where
  (+) = add
  (*) = mul
  negate = (*) (-1)
  abs = id
  signum = const 1
  fromInteger = fst . char

instance (LowHigh a, Eq a) => Eq (DoubleWord a) where
  x == y = (low x == low y) && (high x == high y)

instance (LowHigh a, Ord a) => Ord (DoubleWord a) where
  compare x y = comparing high x y <> comparing low x y

instance (LowHigh a, BigWordArith a) => Semiring (DoubleWord a) where
  zero = extend zero
  char n = (makeLowHigh nl nh, m)
    where
      (nl, m0) = char n
      (nh, m) = char m0

  add x y = fst $ overAdd x y
  mul x y = fst $ overMul x y

instance (LowHigh a, BigWordArith a) => BigWordArith (DoubleWord a) where
  inject = extend . inject
  overAdd x y = (makeLowHigh zl zh, carry)
    where
      (zl, cl) = overAdd (low x) (low y)
      (zh0, ch0) = overAdd (high x) (high y)
      (zh, ch) = overAdd zh0 (inject cl)
      carry = ch + ch0
  overMul = undefined
--  overMul x y = makeLowHigh zl zh
--    where
--      mll = overMul (low x) (low y)
--      mlh = overMul (low x) (high y)
--      mhl = overMul (high x) (low y)
--      mhh = overMul (high x) (high y)
--      mmid0 = overAdd mlh mhl
--      mmidl = shiftExtend $ low $ low mmid0
--      mmidh = makeLowHigh (high $ low mmid0) (low $ high mmid0)
--      zl0 = overAdd mll mmidl
--      zh0 = add mhh mmidh
--      zl = low zl0
--      zh = add (high zl0) zh0

extend :: (LowHigh a, Semiring a) => a -> DoubleWord a
extend = flip makeLowHigh zero

shiftExtend :: (LowHigh a, Semiring a) => a -> DoubleWord a
shiftExtend = makeLowHigh zero

-- | We have a type family for easy parametrization
-- | We have individual type synonyms that are allowed in instances (unlike
-- type synoynm families).
type family BigWord (n :: Nat) where
  BigWord 0 = Word
  -- Instances just for the first few big words, because performance trails
  -- off nonlinearly after this.
  BigWord 1 = BigWord1
  BigWord 2 = BigWord2
  BigWord 3 = BigWord3
  BigWord 4 = BigWord4

-- Word itself needs to be treated specially.

data instance DoubleWord Word = BigWord1 {-# UNPACK #-}!Word {-# UNPACK #-}!Word
instance LowHigh Word where
  low  (BigWord1 x _) = x
  high (BigWord1 _ x) = x
  makeLowHigh = BigWord1

instance Semiring Word where
  zero = 0
  char n = (x, n `shiftR` finiteBitSize x)
    where x = fromInteger n

  add = (+)
  mul = (*)

instance BigWordArith Word where
  inject = id

  overAdd !(W# x#) !(W# y#) = (W# z#, W# c#)
    where (# c#, z# #) = plusWord2# x# y#

  overMul !(W# x#) !(W# y#) = (W# zl#, W# zh#)
    where (# zh#, zl# #) = timesWord2# x# y#

-- The rest are boilerplate

type BigWord1 = DoubleWord Word

data instance DoubleWord BigWord1 = 
  BigWord2 {-# UNPACK #-}!BigWord1 {-# UNPACK #-}!BigWord1
instance LowHigh BigWord1 where
  high (BigWord2 _ x) = x
  low  (BigWord2 x _) = x
  makeLowHigh = BigWord2

type BigWord2 = DoubleWord BigWord1

data instance DoubleWord BigWord2 = 
  BigWord3 {-# UNPACK #-}!BigWord2 {-# UNPACK #-}!BigWord2
instance LowHigh BigWord2 where
  high (BigWord3 _ x) = x
  low  (BigWord3 x _) = x
  makeLowHigh = BigWord3

type BigWord3 = DoubleWord BigWord2

data instance DoubleWord BigWord3 = 
  BigWord4 {-# UNPACK #-}!BigWord3 {-# UNPACK #-}!BigWord3
instance LowHigh BigWord3 where
  high (BigWord4 _ x) = x
  low  (BigWord4 x _) = x
  makeLowHigh = BigWord4

type BigWord4 = DoubleWord BigWord3

-- The general, non-optimized higher types

newtype BoxedBigWord a = BoxedBigWord { getBoxedBigWord :: a }
  deriving (BigWordArith, Semiring)

data instance DoubleWord (BoxedBigWord a) = DoubleWord a a
instance LowHigh (BoxedBigWord a) where
  high (DoubleWord _ x) = BoxedBigWord x
  low  (DoubleWord x _) = BoxedBigWord x
  makeLowHigh = DoubleWord `on` getBoxedBigWord

-- | This bivariate type family gives sequences of big word implementations
-- each based on one of the optimized types above.
type family BigWordStep (s :: Nat) (n :: Nat) where
  BigWordStep s 0 = BigWord s
  BigWordStep s n = DoubleWord (BoxedBigWord (BigWordStep s (n - 1)))

