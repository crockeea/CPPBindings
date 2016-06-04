{-# LANGUAGE ConstraintKinds,
             PolyKinds,
             TypeFamilies, UndecidableInstances
             #-}

module Crypto.Lol.Cyclotomic.Tensor where

import Crypto.Lol.Types.Complex
import Crypto.Lol.Factored
import GHC.Exts

class (TElt t Double, TElt t (Complex Double)) => Tensor t where

  -- | Constraints needed by @t@ to hold type @r@.
  type TElt t r :: Constraint

  -- | Convert a scalar to a tensor in the powerful basis.
  scalarPow :: (Num r, Fact m, TElt t r) => r -> t m r

  -- | 'l' converts from decoding-basis representation to
  -- powerful-basis representation; 'lInv' is its inverse.
  l, lInv :: (Num r, Fact m, TElt t r) => t m r -> t m r
