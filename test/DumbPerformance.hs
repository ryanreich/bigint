{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Data.BigWord
import Data.List
import Data.Time

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let
    n = 
      case args of
        [] -> 1000000
        n:_ -> read n
  let
    a = foldl' (+) 0 $ replicate n (fromInteger @(BigWord 1) $ 3^10)
    b = foldl' (+) 0 $ replicate n ((3 :: Integer)^10)
  t0 <- getCurrentTime
  a `seq` return ()
  t1 <- getCurrentTime
  b `seq` return ()
  t2 <- getCurrentTime
  putStrLn ""
  putStrLn $ "Agreement: " ++ show (a == fromInteger b)
  putStrLn $ "BigWord: " ++ show (diffUTCTime t1 t0)
  putStrLn $ "Integer: " ++ show (diffUTCTime t2 t1)
  return ()
