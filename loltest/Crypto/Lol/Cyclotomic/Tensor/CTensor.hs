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

import Control.Applicative    hiding ((*>))
import Control.Arrow          ((***))

import Data.Proxy
import Data.Coerce
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

import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.Tensor.CTensor.Backend

import System.IO.Unsafe (unsafePerformIO)

-- | Newtype wrapper around a Vector.
newtype CT' r = CT' { unCT :: Vector r } deriving (Show, Eq)

-- | An implementation of 'Tensor' backed by C code.
data CT r where
  CT :: Storable r => CT' r -> CT r

deriving instance Show r => Show (CT r)


wrap :: (Storable r) => (CT' r -> CT' r) -> (CT r -> CT r)
wrap f (CT v) = CT $ f v

instance Tensor CT where

  type TElt CT r = (Storable r, Dispatch r)

  scalarPow = CT . scalarPow' -- Vector code
  l = wrap $ basicDispatch dl
  lInv = wrap $ basicDispatch dlinv

withBasicArgs :: forall r . (Storable r)
  => (Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ())
     -> CT' r -> IO (CT' r)
withBasicArgs f =
  let factors = undefined -- proxy (marshalFactors <$> ppsFact) (Proxy::Proxy m)
      totm = undefined -- proxy (fromIntegral <$> totientFact) (Proxy::Proxy m)
      numFacts = fromIntegral $ SV.length (factors :: Vector Int)
  in \(CT' x) -> do
    yout <- SV.thaw x
    SM.unsafeWith yout (\pout ->
      SV.unsafeWith factors (\pfac ->
        f pout totm pfac numFacts))
    CT' <$> unsafeFreeze yout

basicDispatch :: (Storable r, Num r)
                 => (Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ())
                     -> CT' r -> CT' r
basicDispatch f = unsafePerformIO . withBasicArgs f

scalarPow' :: forall  r . (Num r, Storable r) => r -> CT' r
-- constant-term coefficient is first entry wrt powerful basis
scalarPow' =
  let n = undefined -- proxy totientFact (Proxy::Proxy m)
  in \r -> CT' $ generate n (\i -> if i == 0 then r else 0)
