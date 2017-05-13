{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.BigInt
  (

  ) where

import GHC.Prim
import GHC.Types

type BigWord# (lowHalf# :: TYPE 'UnboxedTupleRep) (highHalf# :: TYPE 'UnboxedTupleRep) = (# lowHalf#, highHalf# #)

class WordArith# (a# :: TYPE 'UnboxedTupleRep) where
  add# :: a# -> a# -> a#

class BigWordArith# (b# :: TYPE 'UnboxedTupleRep) (a# :: TYPE 'UnboxedTupleRep) where
  overAdd# :: a# -> a# -> (# a#, b# #)

instance BigWordArith# (# Word# #) (# Word# #) where
  overAdd# (# x# #) (# y# #) = (# (# sl# #), (# sh# #) #)
    where (# sh#, sl# #) = plusWord2# x# y#

instance (BigWordArith# (# ah# #) (# al# #), BigWordArith# (# b# #) (# ah# #), WordArith# (# b# #)) => BigWordArith# (# b# #) (BigWord# (# al# #) (# ah# #)) where
  overAdd# (# xl#, xh# #) (# yl#, yh# #) = (# (# zl#, zh# #), w# #)
    where
      (# zl#, zh0# #) = overAdd# xl# yl#
      (# zh1#, w0# #) = overAdd# xh# yh#
      (# zh#, w1# #) = overAdd# zh0# zh1#
      w# = add# w0# w1#
