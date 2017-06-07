{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.BigWordClass
  (
    LowHigh(..)
  ) where

import Data.Bits
import Data.Monoid
import Data.Ord

import GHC.Prim
import GHC.Types

-- | @DoubleWord a ~ (a, a)@
class LowHigh a where
  -- | Must be a data and not a type so that we can define instances
  data DoubleWord a
  low :: DoubleWord a -> a
  high :: DoubleWord a -> a
  makeLowHigh :: a -> a -> DoubleWord a

-- | A subset of the `Num` operations
class Semiring a where
  zero :: a
  one :: a
  -- | This is actually a fancy version of the unit 1, since the
  -- characteristic function is just integers interpreted as multiples of
  -- 1.
  char :: Integer -> (a, Integer)

  add :: a -> a -> a
  mul :: a -> a -> a

-- | Arithmetic with carrying
class (Semiring a, LowHigh a) => BigWordArith a where
  overAdd :: a -> a -> DoubleWord a
  overMul :: a -> a -> DoubleWord a

instance {-# OVERLAPPABLE #-} (Semiring a) => Num a where
  (+) = add
  (*) = mul
  negate = (*) (-1)
  abs = id
  signum = const 1
  fromInteger = fst . char

instance LowHigh Word where
  data DoubleWord Word = BigWord1 {-# UNPACK #-}!Word {-# UNPACK #-}!Word
  low  (BigWord1 x _) = x
  high (BigWord1 _ x) = x
  makeLowHigh = BigWord1

instance Semiring Word where
  zero = 0
  one = 1
  char n = (x, n `shiftR` finiteBitSize x)
    where x = fromInteger n

  add = (+)
  mul = (*)

instance BigWordArith Word where
  overAdd !(W# x#) !(W# y#) = makeLowHigh (W# zl#) (W# zh#)
    where (# zh#, zl# #) = plusWord2# x# y#
  overMul !(W# x#) !(W# y#) = makeLowHigh (W# zl#) (W# zh#)
    where (# zh#, zl# #) = timesWord2# x# y#

instance (LowHigh a, Eq a) => Eq (DoubleWord a) where
  x == y = (low x == low y) && (high x == high y)

instance (LowHigh a, Ord a) => Ord (DoubleWord a) where
  compare x y = comparing high x y <> comparing low x y

instance (Ord a, Semiring a, LowHigh a, LowHigh (DoubleWord a)) => Semiring (DoubleWord a) where
  zero = extend zero
  one = extend 1
  char n = (makeLowHigh nl nh, m)
    where
      (nl, m0) = char n
      (nh, m) = char m0

  add x y = fst $ lazyOverAdd x y
  mul x y = low $ overMul x y

instance (Ord a, Semiring a, LowHigh a, LowHigh (DoubleWord a)) => BigWordArith (DoubleWord a) where
  overAdd x y = makeLowHigh z (if c then one else zero)
    where (z, c) = lazyOverAdd x y

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

{-# INLINE lazyOverAdd #-}
lazyOverAdd :: 
  (Ord a, Semiring a, LowHigh a) => 
  DoubleWord a -> DoubleWord a -> (DoubleWord a, Bool)
lazyOverAdd x y = (makeLowHigh zl zh, carry)
  where
    zl = add (low x) (low y)
    zh0 = add (high x) (high y)
    zh = (if zl < low x then add one else id) zh0
    carry = zh < zh0 || zh0 < high x

