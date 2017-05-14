{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.BigWordUnpack 
  (
    BigWord, BigWord1, makeBigWordsTo
  ) where

import Data.BigWordClass

import GHC.TypeLits

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | We have a type family for easy parametrization
type family BigWord (n :: Nat)
type instance BigWord 0 = Word
type instance BigWord 1 = BigWord1

-- | We have individual type synonyms that are allowed in instances (unlike
-- type synoynm families).
type BigWord1 = DoubleWord Word

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
