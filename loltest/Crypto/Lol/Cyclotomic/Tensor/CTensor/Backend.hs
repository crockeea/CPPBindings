{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, PolyKinds, ScopedTypeVariables,
             TypeFamilies, UndecidableInstances #-}

-- | This module contains the functions to transform Haskell types into their
-- C counterpart, and to transform polymorphic Haskell functions into C funtion
-- calls in a type-safe way.

module Crypto.Lol.Cyclotomic.Tensor.CTensor.Backend
( Dispatch
, dcrt, dcrtinv
, dl, dlinv
, dnorm
, dmulgpow, dmulgdec
, dginvpow, dginvdec
, dmul
, CPP
, withArray, withPtrArray
) where

import Control.Applicative

import Data.Proxy
import Data.Int
import Data.Vector.Storable          as SV (Vector, fromList,
                                            unsafeToForeignPtr0)
import Data.Vector.Storable.Internal (getPtr)

import           Foreign.ForeignPtr      (touchForeignPtr)
import           Foreign.Marshal.Array   (withArray)
import           Foreign.Marshal.Utils   (with)
import           Foreign.Ptr             (Ptr, castPtr, plusPtr)
import           Foreign.Storable        (Storable (..))

-- http://stackoverflow.com/questions/6517387/vector-vector-foo-ptr-ptr-foo-io-a-io-a
-- | Evaluates a C function that takes a @a** ptr@ on a list of Vectors.
withPtrArray :: (Storable a) => [Vector a] -> (Ptr (Ptr a) -> IO b) -> IO b
withPtrArray v f = do
  let vs = map SV.unsafeToForeignPtr0 v
      ptrV = map (\(fp,_) -> getPtr fp) vs
  res <- withArray ptrV f
  mapM_ (\(fp,_) -> touchForeignPtr fp) vs
  return res

-- | C representation of a prime power.
data CPP = CPP {p' :: !Int32, e' :: !Int16}
-- stolen from http://hackage.haskell.org/packages/archive/numeric-prelude/0.4.0.3/doc/html/src/Number-Complex.html#T
-- the NumericPrelude Storable instance for complex numbers
instance Storable CPP

instance Show CPP where
    show (CPP p e) = "(" ++ show p ++ "," ++ show e ++ ")"

instance (Storable a, Storable b,
          CTypeOf a ~ CTypeOf b)
  -- enforces right associativity and that each type of
  -- the tuple has the same C repr, so using an array repr is safe
  => Storable (a,b) where
  sizeOf _ = (sizeOf (undefined :: a)) + (sizeOf (undefined :: b))
  alignment _ = max (alignment (undefined :: a)) (alignment (undefined :: b))
  peek p = do
    a <- peek (castPtr p :: Ptr a)
    b <- peek (castPtr (plusPtr p (sizeOf a)) :: Ptr b)
    return (a,b)
  poke p (a,b) = do
    poke (castPtr p :: Ptr a) a
    poke (castPtr (plusPtr p (sizeOf a)) :: Ptr b) b

data DoubleD
data Int64D

type family CTypeOf x where
  CTypeOf (a,b) = CTypeOf a
  CTypeOf Double = DoubleD
  CTypeOf Int64 = Int64D

-- | Single-argument synonym for @Dispatch'@.
type Dispatch r = (Dispatch' (CTypeOf r) r)

-- | Class to safely match Haskell types with the appropriate C function.
class (repr ~ CTypeOf r) => Dispatch' repr r where
  -- | Equivalent to 'Tensor's @crt@.
  dcrt      :: Ptr (Ptr r) ->           Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @crtInv@.
  dcrtinv   :: Ptr (Ptr r) -> Ptr r ->  Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @l@.
  dl        :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @lInv@.
  dlinv     :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @gSqNormDec@.
  dnorm     :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @mulGPow@.
  dmulgpow  :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @mulGDec@.
  dmulgdec  :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @divGPow@.
  dginvpow  :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @divGDec@.
  dginvdec  :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to @zipWith (*)@
  dmul :: Ptr r -> Ptr r -> Int64 -> IO ()

instance (CTypeOf r ~ DoubleD) => Dispatch' DoubleD r where
  dcrt = error "cannot call CT Crt on type Double"
  dcrtinv = error "cannot call CT CrtInv on type Double"
  dl pout =
    tensorLDouble (1) (castPtr pout)
  dlinv pout =
    tensorLInvDouble (1) (castPtr pout)
  dnorm pout = tensorNormSqD (1) (castPtr pout)
  dmulgpow = error "cannot call CT mulGPow on type Double"
  dmulgdec = error "cannot call CT mulGDec on type Double"
  dginvpow = error "cannot call CT divGPow on type Double"
  dginvdec = error "cannot call CT divGDec on type Double"
  dmul = error "cannot call CT (*) on type Double"

instance (CTypeOf r ~ Int64D) => Dispatch' Int64D r where
  dcrt = error "cannot call CT Crt on type Int64"
  dcrtinv = error "cannot call CT CrtInv on type Int64"
  dl pout =
    tensorLR (1) (castPtr pout)
  dlinv pout =
    tensorLInvR (1) (castPtr pout)
  dnorm pout =
    tensorNormSqR (1) (castPtr pout)
  dmulgpow pout =
    tensorGPowR (1) (castPtr pout)
  dmulgdec pout =
    tensorGDecR (1) (castPtr pout)
  dginvpow pout =
    tensorGInvPowR (1) (castPtr pout)
  dginvdec pout =
    tensorGInvDecR (1) (castPtr pout)
  dmul = error "cannot call CT (*) on type Int64"

foreign import ccall unsafe "tensorLR" tensorLR ::                  Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLInvR" tensorLInvR ::            Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLDouble" tensorLDouble ::       Int16 -> Ptr Double -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLInvDouble" tensorLInvDouble :: Int16 -> Ptr Double -> Int64 -> Ptr CPP -> Int16          -> IO ()

foreign import ccall unsafe "tensorNormSqR" tensorNormSqR ::     Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorNormSqD" tensorNormSqD ::     Int16 -> Ptr Double -> Int64 -> Ptr CPP -> Int16          -> IO ()

foreign import ccall unsafe "tensorGPowR" tensorGPowR ::         Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGDecR" tensorGDecR ::         Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGInvPowR" tensorGInvPowR ::   Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGInvDecR" tensorGInvDecR ::   Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()