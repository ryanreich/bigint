{-# LANGUAGE TypeApplications #-}
module Main where

import Data.BigWord
import Criterion.Main

main :: IO ()
main = defaultMain
  [
    env twoNumbers $ \ ~(x, y) -> bgroup "BigWord/Integer comparision"
      [
        bgroup "BigWord"
          [
            bench "BigWord 0" $ whnf (uncurry (+) @(BigWord 0)) (fromInteger x, fromInteger y)
            bench "BigWord 1" $ whnf (uncurry (+) @(BigWord 1)) (fromInteger x, fromInteger y)
            bench "BigWord 2" $ whnf (uncurry (+) @(BigWord 2)) (fromInteger x, fromInteger y)
            bench "BigWord 3" $ whnf (uncurry (+) @(BigWord 3)) (fromInteger x, fromInteger y)
            bench "BigWord 4" $ whnf (uncurry (+) @(BigWord 4)) (fromInteger x, fromInteger y)
            bench "BigWord 5" $ whnf (uncurry (+) @(BigWord 5)) (fromInteger x, fromInteger y)
          ],
        bench "Integer" $ whnf (uncurry (+)) (x, y)
      ]
  ]

twoNumbers :: IO (Integer, Integer)
twoNumbers = return (1,1)
