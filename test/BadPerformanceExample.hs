{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Main where

import Data.Bits
import Data.List
import Data.Time

import GHC.Prim
import GHC.Word

data BW1 = BW1 {-# UNPACK #-}!Word {-# UNPACK #-}!Word
data BW2 = BW2 {-# UNPACK #-}!BW1  {-# UNPACK #-}!BW1

overAddWord !(W# x#) !(W# y#) = (W# z#, W# c#)
  where (# c#, z# #) = plusWord2# x# y#

overAddBW1 (BW1 x1 y1) (BW1 x2 y2) = (BW1 s $ t + cs, c)
  where 
    (s, cs) = overAddWord x1 x2
    (t0, ct0) = overAddWord y1 y2
    (t, ct) = overAddWord t0 cs    
    c = ct + ct0
addBW1 x y = fst $ overAddBW1 x y
overAddBW2 (BW2 x1 y1) (BW2 x2 y2) = (BW2 s t', c)
  where 
    (s, cs) = overAddBW1 x1 x2
    (t, ct) = overAddBW1 y1 y2
    (t', ct') = overAddBW1 t $ BW1 cs 0
    c = ct .|. ct'
addBW2 x y = z
  where (z, _) = overAddBW2 x y

main = do
  let
    bw1 i = BW1 i i
    bw2 i = BW2 (bw1 i) (bw1 i)
    a1 = foldl' addBW1 (bw1 0) $ replicate 1000000 (bw1 1)
    a2 = foldl' addBW2 (bw2 0) $ replicate 1000000 (bw2 1)
    a3 = foldl' (+) 0 $ replicate 1000000 1
  t0 <- getCurrentTime
  a1 `seq` return ()
  t1 <- getCurrentTime
  a2 `seq` return ()
  t2 <- getCurrentTime
  a3 `seq` return ()
  t3 <- getCurrentTime
  putStrLn ""
  putStrLn $ "BW1:     " ++ show (diffUTCTime t1 t0)
  putStrLn $ "BW2:     " ++ show (diffUTCTime t2 t1)
  putStrLn $ "Integer: " ++ show (diffUTCTime t3 t2)

