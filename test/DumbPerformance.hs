{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.BigWord
import Data.List
import Data.Time

import System.Environment

makeBigWordsTo 3

main :: IO ()
main = do
  args <- getArgs
  let
    n = 
      case args of
        [] -> 1000000
        n:_ -> read n
  let
    a1 = foldl' (+) 0 $ replicate n (fromInteger @(BigWord 1) $ 3^10)
    a2 = foldl' (+) 0 $ replicate n (fromInteger @(BigWord 2) $ 3^10)
    a3 = foldl' (+) 0 $ replicate n (fromInteger @(BigWord 3) $ 3^10)
    b = foldl' (+) 0 $ replicate n ((3 :: Integer)^10)
  t0 <- getCurrentTime
  a1 `seq` return ()
  t1 <- getCurrentTime
  a2 `seq` return ()
  t2 <- getCurrentTime
  a3 `seq` return ()
  t3 <- getCurrentTime
  b `seq` return ()
  t4 <- getCurrentTime
  putStrLn ""
  putStrLn $ "Agreement: " ++ show (a1 == fromInteger b && a2 == fromInteger b && a3 == fromInteger b)
  putStrLn $ "BigWord1: " ++ show (diffUTCTime t1 t0)
  putStrLn $ "BigWord2: " ++ show (diffUTCTime t2 t1)
  putStrLn $ "BigWord3: " ++ show (diffUTCTime t3 t2)
  putStrLn $ "Integer : " ++ show (diffUTCTime t4 t3)
  return ()
