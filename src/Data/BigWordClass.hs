{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.BigWordClass
  (
    LowHigh(..), WordArith(..)
  ) where

import GHC.Prim
import GHC.Types

-- | @DoubleWord a ~ (a, a)@
class LowHigh a where
  data DoubleWord a
  low :: DoubleWord a -> a
  high :: DoubleWord a -> a
  makeLowHigh :: a -> a -> DoubleWord a

-- | A subset of the `Num` operations
class WordArith a where
  add :: a -> a -> a
  mul :: a -> a -> a

-- | Arithmetic with carrying
class (WordArith a, LowHigh a) => BigWordArith a where
  extend :: a -> DoubleWord a
  shiftExtend :: a -> DoubleWord a
  overAdd :: a -> a -> DoubleWord a
  overMul :: a -> a -> DoubleWord a

instance LowHigh Word where
  data DoubleWord Word = BigWord1 {-# UNPACK #-}!Word {-# UNPACK #-}!Word
  low  (BigWord1 x _) = x
  high (BigWord1 _ x) = x
  makeLowHigh = BigWord1

instance WordArith Word where
  add = (+)
  mul = (*)

instance BigWordArith Word where
  extend = flip makeLowHigh 0
  shiftExtend = makeLowHigh 0
  overAdd !(W# x#) !(W# y#) = makeLowHigh (W# zl#) (W# zh#)
    where (# zh#, zl# #) = plusWord2# x# y#
  overMul !(W# x#) !(W# y#) = makeLowHigh (W# zl#) (W# zh#)
    where (# zh#, zl# #) = timesWord2# x# y#

instance (BigWordArith a, LowHigh (DoubleWord a)) => WordArith (DoubleWord a) where
  add x y = low $ overAdd x y
  mul x y = low $ overMul x y

instance (BigWordArith a, LowHigh (DoubleWord a)) => BigWordArith (DoubleWord a) where
  extend x = makeLowHigh x $ zero2 $ low x

  shiftExtend x = flip makeLowHigh x $ zero2 $ low x

  overAdd x y = makeLowHigh (makeLowHigh zl zh) (extend w)
    where
      z1 = overAdd (low x) (low y)
      z2 = overAdd (high x) (high y)
      z3 = overAdd (high z1) (low z2)
      zl = low z1
      zh = low z3
      w = add (high z2) (high z3)

  overMul x y = makeLowHigh zl zh
    where
      mll = overMul (low x) (low y)
      mlh = overMul (low x) (high y)
      mhl = overMul (high x) (low y)
      mhh = overMul (high x) (high y)
      mmid0 = overAdd mlh mhl
      mmidl = shiftExtend $ low $ low mmid0
      mmidh = makeLowHigh (high $ low mmid0) (low $ high mmid0)
      zl0 = overAdd mll mmidl
      zh0 = add mhh mmidh
      zl = low zl0
      zh = add (high zl0) zh0

zero :: (BigWordArith a, LowHigh a) => a -> a
zero = high . extend

zero2 :: (BigWordArith a, LowHigh a) => a -> DoubleWord a
zero2 = extend . zero

