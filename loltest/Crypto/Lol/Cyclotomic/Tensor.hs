{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             MultiParamTypeClasses, NoImplicitPrelude, PolyKinds,
             RankNTypes, ScopedTypeVariables, TupleSections, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

-- | Interface for cyclotomic tensors, and helper functions for tensor
-- indexing.

module Crypto.Lol.Cyclotomic.Tensor where

import Crypto.Lol.CRTrans
import Crypto.Lol.Prelude           as LP hiding (lift, (*>))
import Crypto.Lol.Types.FiniteField

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad.Random
import           Data.Constraint
import           Data.Singletons.Prelude hiding ((:-))
import           Data.Traversable
import           Data.Tuple              (swap)
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as U

-- | 'Tensor' encapsulates all the core linear transformations needed
-- for cyclotomic ring arithmetic.

-- | The type @t m r@ represents a cyclotomic coefficient tensor of
-- index @m@ over base ring @r@.  Most of the methods represent linear
-- transforms corresponding to operations in particular bases.
-- CRT-related methods are wrapped in 'Maybe' because they are
-- well-defined only when a CRT basis exists over the ring @r@ for
-- index @m@.

-- | The superclass constraints are for convenience, to ensure that we
-- can sample error tensors of 'Double's.

-- | __WARNING:__ as with all fixed-point arithmetic, the methods
-- in 'Tensor' may result in overflow (and thereby incorrect answers
-- and potential security flaws) if the input arguments are too close
-- to the bounds imposed by the base type.  The acceptable range of
-- inputs for each method is determined by the linear transform it
-- implements.

class (TElt t Double, TElt t (Complex Double)) => Tensor t where

  -- | Constraints needed by @t@ to hold type @r@.
  type TElt t r :: Constraint

  -- | Convert a scalar to a tensor in the powerful basis.
  scalarPow :: (Additive r, Fact m, TElt t r) => r -> t m r

  -- | 'l' converts from decoding-basis representation to
  -- powerful-basis representation; 'lInv' is its inverse.
  l, lInv :: (Additive r, Fact m, TElt t r) => t m r -> t m r
