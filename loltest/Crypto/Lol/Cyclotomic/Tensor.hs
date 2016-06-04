{-# LANGUAGE ConstraintKinds,
             PolyKinds,
             TypeFamilies, UndecidableInstances
             #-}

module Crypto.Lol.Cyclotomic.Tensor where

import GHC.Exts

class (TElt t Double) => Tensor t where

  -- | Constraints needed by @t@ to hold type @r@.
  type TElt t r :: Constraint

  -- | Convert a scalar to a tensor in the powerful basis.
  scalarPow :: (Num r, TElt t r) => r -> t r

  -- | 'l' converts from decoding-basis representation to
  -- powerful-basis representation; 'lInv' is its inverse.
  l, lInv :: (Num r, TElt t r) => t r -> t r
