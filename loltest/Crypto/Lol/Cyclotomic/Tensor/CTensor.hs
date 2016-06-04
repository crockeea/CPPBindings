{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, GADTs, GeneralizedNewtypeDeriving,
             InstanceSigs, MultiParamTypeClasses,
             PolyKinds, RankNTypes, RoleAnnotations,
             ScopedTypeVariables, StandaloneDeriving, TupleSections,
             TypeFamilies, TypeOperators, TypeSynonymInstances,
             UndecidableInstances #-}

-- | Wrapper for a C implementation of the 'Tensor' interface.

module Crypto.Lol.Cyclotomic.Tensor.CTensor
( CT ) where

import Algebra.Additive     as Additive (C)
import Algebra.Module       as Module (C)
import Algebra.ZeroTestable as ZeroTestable (C)

import Control.Applicative    hiding ((*>))
import Control.Arrow          ((***))
import Control.DeepSeq
import Control.Monad.Except
import Control.Monad.Identity (Identity (..), runIdentity)
import Control.Monad.Random
import Control.Monad.Trans    as T (lift)

import Data.Proxy
import Data.Functor.Trans.Tagged
import Data.Coerce
import Data.Constraint              hiding ((***))
import Data.Int
import Data.Maybe
import Data.Traversable             as T
import Data.Vector.Generic          as V (fromList, toList, unzip)
import Data.Vector.Storable         as SV (Vector, convert, foldl',
                                           foldl1', fromList, generate,
                                           length, map, mapM, replicate,
                                           replicateM, thaw, thaw, toList,
                                           unsafeFreeze, unsafeSlice,
                                           unsafeWith, zipWith, (!))
import Data.Vector.Storable.Mutable as SM hiding (replicate)

import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Test.QuickCheck       hiding (generate)

import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.Tensor.CTensor.Backend
import Crypto.Lol.Factored

import System.IO.Unsafe (unsafePerformIO)

-- | Newtype wrapper around a Vector.
newtype CT' (m :: Factored) r = CT' { unCT :: Vector r }
                              deriving (Show, Eq, NFData)

-- the first argument, though phantom, affects representation
type role CT' representational nominal

-- | An implementation of 'Tensor' backed by C code.
data CT (m :: Factored) r where
  CT :: Storable r => CT' m r -> CT m r

deriving instance Show r => Show (CT m r)


wrap :: (Storable r) => (CT' l r -> CT' m r) -> (CT l r -> CT m r)
wrap f (CT v) = CT $ f v

instance Tensor CT where

  type TElt CT r = (Storable r, Dispatch r)

  scalarPow = CT . scalarPow' -- Vector code
  l = wrap $ untag $ basicDispatch dl
  lInv = wrap $ untag $ basicDispatch dlinv

withBasicArgs :: forall m r . (Fact m, Storable r)
  => (Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ())
     -> CT' m r -> IO (CT' m r)
withBasicArgs f =
  let factors = proxy (marshalFactors <$> ppsFact) (Proxy::Proxy m)
      totm = proxy (fromIntegral <$> totientFact) (Proxy::Proxy m)
      numFacts = fromIntegral $ SV.length factors
  in \(CT' x) -> do
    yout <- SV.thaw x
    SM.unsafeWith yout (\pout ->
      SV.unsafeWith factors (\pfac ->
        f pout totm pfac numFacts))
    CT' <$> unsafeFreeze yout

basicDispatch :: (Storable r, Fact m, Num r)
                 => (Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ())
                     -> Tagged m (CT' m r -> CT' m r)
basicDispatch f = return $ unsafePerformIO . withBasicArgs f

scalarPow' :: forall m r . (Fact m, Num r, Storable r) => r -> CT' m r
-- constant-term coefficient is first entry wrt powerful basis
scalarPow' =
  let n = proxy totientFact (Proxy::Proxy m)
  in \r -> CT' $ generate n (\i -> if i == 0 then r else 0)
