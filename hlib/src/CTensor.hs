{-# LANGUAGE FlexibleContexts,
             TypeFamilies,
             UndecidableInstances #-}

module CTensor
( CT, temp ) where

import Backend
import GHC.Exts
import Data.Int

newtype CT' r = CT' [r] deriving (Show)
newtype CT  r = CT  (CT' r) deriving (Show)

instance Tensor CT where
  type TElt CT r = (Dispatch r)

scalarPow' :: r -> CT' r
scalarPow' x = CT' [x]

temp :: Int64 -> CT Int64
temp = CT. scalarPow'

class (TElt t Double) => Tensor t where
  type TElt t r :: Constraint
  scalarPow :: (Num r, TElt t r) => r -> t r
