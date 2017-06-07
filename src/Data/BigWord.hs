{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.BigWord 
  (
    module Data.BigWordUnpack,
    module Data.BigWord
  ) where

import Data.BigWordUnpack

makeBigWordsTo 5
