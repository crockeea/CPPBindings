{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             MultiParamTypeClasses, NoImplicitPrelude, PolyKinds,
             RankNTypes, ScopedTypeVariables, TupleSections, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

-- | Interface for cyclotomic tensors, and helper functions for tensor
-- indexing.

module Crypto.Lol.Cyclotomic.Tensor where

import Crypto.Lol.CRTrans
import Crypto.Lol.Prelude           as LP hiding (lift, (*>))

import           Data.Constraint

class (TElt t Double, TElt t (Complex Double)) => Tensor t where

  -- | Constraints needed by @t@ to hold type @r@.
  type TElt t r :: Constraint

  -- | Convert a scalar to a tensor in the powerful basis.
  scalarPow :: (Additive r, Fact m, TElt t r) => r -> t m r

  -- | 'l' converts from decoding-basis representation to
  -- powerful-basis representation; 'lInv' is its inverse.
  l, lInv :: (Additive r, Fact m, TElt t r) => t m r -> t m r
