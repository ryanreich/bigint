{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Criterion.Main

import Data.BigWord
import Data.Proxy

import GHC.TypeLits

main :: IO ()
main = defaultMain
  [
    bgroup "BigWord-Integer comparison"
      [
        bigWordIntegerTest 0 $ Proxy @(BigWord 0),
        bigWordIntegerTest 1 $ Proxy @(BigWord 1),
        bigWordIntegerTest 2 $ Proxy @(BigWord 2),
        bigWordIntegerTest 3 $ Proxy @(BigWord 3),
        bigWordIntegerTest 4 $ Proxy @(BigWord 4),
        bigWordIntegerTest 5 $ Proxy @(BigWordStep 4 1),
        bigWordIntegerTest 6 $ Proxy @(BigWordStep 4 2)
      ]
  ]

bigWordIntegerTest :: forall t. (Num t) => Integer -> Proxy t -> Benchmark
bigWordIntegerTest n zero =
  env (twoNumbers n) $ \ ~(x, y) -> bgroup ("BigWord " ++ show n)
    [
      bench "BigWord" $ whnf (uncurry (+)) (fromInteger x :: t, fromInteger y :: t),
      bench "Integer" $ whnf (uncurry (+)) (x, y)
    ]

twoNumbers :: Integer -> IO (Integer, Integer)
twoNumbers n = return (x,x)
  where x = 2^(64 * 2^n) - 1
