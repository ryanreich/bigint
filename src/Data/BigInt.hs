{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.BigInt
  (

  ) where

import GHC.Prim

data BigKey lowHalf# highHalf# = BigKey# (# lowHalf#, highHalf# #)

class Num# a# where
  (+#) :: a# -> a# -> a#
  (-#) :: a# -> a# -> a#
  (*#) :: a# -> a# -> a#

  negate# :: a# -> a#
  abs# :: a# -> a#
  signum# :: a# -> a#

  fromInteger# :: Integer -> a#

instance Num# Word# where
  (+#) = plusWord#
  (-#) = minusWord#
  (*#) = timesWord#

  negate# = narrow8Word# . int2Word# . negateInt# . word2Int# 
  abs# = id
  signum# = id#

  fromInteger# = undefined

--instance Num# Word32# where
--
--instance Num# Word64# where

-- instance 
--   (
--     Num# lowHalf#, 
--     Num# highHalf#
--   ) =>  
--   Num# (BigKey lowHalf# highHalf#) where

