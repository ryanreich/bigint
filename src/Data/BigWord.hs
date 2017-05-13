{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.BigWord
  (
  ) where

import GHC.Prim
import GHC.Types

type BigWord# (halfWord# :: TYPE 'UnboxedTupleRep) = (# halfWord#, halfWord# #)

class WordArith# (a# :: TYPE 'UnboxedTupleRep) where
  add# :: a# -> a# -> a#
  mul# :: a# -> a# -> a#

instance WordArith# (# Word# #) where
  add# (# x# #) (# y# #) = (# plusWord# x# y# #)
  mul# (# x# #) (# y# #) = (# timesWord# x# y# #)

instance (BigWordArith# (# a# #)) => WordArith# (# BigWord# (# a# #) #) where
  add# x# y# = z#
    where (# z#, _ #) = overAdd# x# y#

  mul# x# y# = z#
    where (# z#, _ #) = overMul# x# y#

class (WordArith# a#) => BigWordArith# (a# :: TYPE 'UnboxedTupleRep) where
  extend# :: a# -> BigWord# a#
  shiftExtend# :: a# -> BigWord# a#
  overAdd# :: a# -> a# -> BigWord# a#
  overMul# :: a# -> a# -> BigWord# a#

instance BigWordArith# (# Word# #) where
  extend# (# x# #) = (# (# x# #), (# 0## #) #)

  shiftExtend# (# x# #) = (# (# 0## #), (# x# #) #)

  overAdd# (# x# #) (# y# #) = (# (# sl# #), (# sh# #) #)
    where (# sh#, sl# #) = plusWord2# x# y#

  overMul# (# x# #) (# y# #) = (# (# pl# #), (# ph# #) #)
    where (# ph#, pl# #) = timesWord2# x# y#

instance (BigWordArith# (# a# #)) => BigWordArith# (# BigWord# (# a# #) #) where
  extend# (# (# x#, y# #) #) = (# (# (# x#, y# #) #), (# (# z#, z# #) #) #)
    where (# _, z# #) = extend# x#

  shiftExtend# (# (# x#, y# #) #) = (# (# (# z#, z# #) #), (# (# x#, y# #) #) #)
    where (# z#, _ #) = shiftExtend# x#

  overAdd# (# (# xl#, xh# #) #) (# (# yl#, yh# #) #) = (# (# (# zl#, zh# #) #), (# (# wl#, wh# #) #) #)
    where
      (# zl#, zh0# #) = overAdd# xl# yl#
      (# zh1#, w0# #) = overAdd# xh# yh#
      (# zh#, w1# #) = overAdd# zh0# zh1#
      w# = add# w0# w1#
      (# wl#, wh# #) = extend# w#

  overMul# (# (# xl#, xh# #) #) (# (# yl#, yh# #) #) = (# zl#, zh# #)
    where
      mll# = overMul# xl# yl#
      mlh# = overMul# xl# yh#
      mhl# = overMul# xh# yl#
      mhh# = overMul# xh# yh#
      mlowhigh# = (# (# (# mll# #), (# mhh# #) #) #)
      (# (# (# mmidll#, mmidlh# #) #), (# (# mmidhl#, _ #) #) #) = overAdd# (# mlh# #) (# mhl# #)
      (# zl#, w0# #) = overAdd# (# mll# #) (# shiftExtend# mmidll# #)
      zh0# = add# (# mhh# #) (# (# mmidlh#, mmidhl# #) #)
      zh# = add# w0# zh0#

