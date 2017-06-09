{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.BigWord where

import Data.Bits
import Data.Monoid
import Data.Ord

import GHC.Prim
import GHC.TypeLits
import GHC.Types

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | We have a type family for easy parametrization
type family BigWord (n :: Nat)
type instance BigWord 0 = Word
type instance BigWord 1 = BigWord1

-- | We have individual type synonyms that are allowed in instances (unlike
-- type synoynm families).
type BigWord1 = DoubleWord Word

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
  inject = id

  overAdd !(W# x#) !(W# y#) = (W# z#, W# c#)
    where (# c#, z# #) = plusWord2# x# y#

  overMul !(W# x#) !(W# y#) = (W# zl#, W# zh#)
    where (# zh#, zl# #) = timesWord2# x# y#

instance (LowHigh a, Eq a) => Eq (DoubleWord a) where
  x == y = (low x == low y) && (high x == high y)

instance (LowHigh a, Ord a) => Ord (DoubleWord a) where
  compare x y = comparing high x y <> comparing low x y

instance (LowHigh a, BigWordArith a) => Semiring (DoubleWord a) where
  zero = extend zero
  one = extend 1
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

-- | See the definition of @BigWord 1@ and its dependencies for what this
-- does.
makeBigWordsTo :: Integer -> Q [Dec]
makeBigWordsTo n | n > 1 = 
  return $ flip concatMap [2 .. n] $ \i ->
    let 
      dataName j = mkName $ "BigWord" ++ show j 
      dataN = dataName i
      dataN' = dataName $ i - 1
      xName = mkName "x"
    in
    [
      TySynInstD ''BigWord $ TySynEqn [LitT $ NumTyLit i] (ConT dataN),
      TySynD dataN [] $ ConT ''DoubleWord `AppT` ConT dataN',
      InstanceD Nothing [] (ConT ''LowHigh `AppT` ConT dataN')
        [
          DataInstD [] ''DoubleWord [ConT dataN'] Nothing 
            [
              NormalC dataN
                [
                  (Bang SourceUnpack SourceStrict, ConT $ dataN'),
                  (Bang SourceUnpack SourceStrict, ConT $ dataN')
                ]
            ]
            [],
          FunD 'high 
            [Clause [ConP dataN [WildP, VarP xName]] (NormalB $ VarE xName) []],
          FunD 'low 
            [Clause [ConP dataN [VarP xName, WildP]] (NormalB $ VarE xName) []],
          FunD 'makeLowHigh 
            [Clause [] (NormalB $ ConE dataN) []]
        ]
    ]
makeBigWordsTo _ = return []
