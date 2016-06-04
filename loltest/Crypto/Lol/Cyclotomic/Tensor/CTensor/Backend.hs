{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, PolyKinds, ScopedTypeVariables,
             TypeFamilies, UndecidableInstances #-}

-- | This module contains the functions to transform Haskell types into their
-- C counterpart, and to transform polymorphic Haskell functions into C funtion
-- calls in a type-safe way.

module Crypto.Lol.Cyclotomic.Tensor.CTensor.Backend
( Dispatch
, dcrt, dcrtinv
, dgaussdec
, dl, dlinv
, dnorm
, dmulgpow, dmulgdec
, dginvpow, dginvdec
, dmul
, marshalFactors
, CPP
, withArray, withPtrArray
) where

import Control.Applicative
import Crypto.Lol.Factored
import Crypto.Lol.Types.Complex
import Crypto.Lol.Reflects

import Data.Proxy
import Data.Functor.Trans.Tagged
import Data.Int
import Data.Vector.Storable          as SV (Vector, fromList,
                                            unsafeToForeignPtr0)
import Data.Vector.Storable.Internal (getPtr)

import           Foreign.ForeignPtr      (touchForeignPtr)
import           Foreign.Marshal.Array   (withArray)
import           Foreign.Marshal.Utils   (with)
import           Foreign.Ptr             (Ptr, castPtr, plusPtr)
import           Foreign.Storable        (Storable (..))
import qualified Foreign.Storable.Record as Store

-- | Convert a list of prime powers to a suitable C representation.
marshalFactors :: [PP] -> Vector CPP
marshalFactors = SV.fromList . map (\(p,e) -> CPP (fromIntegral p) (fromIntegral e))

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
instance Storable CPP where
   sizeOf    = Store.sizeOf store
   alignment = Store.alignment store
   peek      = Store.peek store
   poke      = Store.poke store

store :: Store.Dictionary CPP
store = Store.run $
   liftA2 CPP
      (Store.element p')
      (Store.element e')

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





data ComplexD
data DoubleD
data Int64D

type family CTypeOf x where
  CTypeOf (a,b) = CTypeOf a
  CTypeOf Double = DoubleD
  CTypeOf Int64 = Int64D
  CTypeOf (Complex Double) = ComplexD

-- returns the modulus as a nested list of moduli
class (Tuple a) => ZqTuple a where
  type ModPairs a
  getModuli :: Tagged a (ModPairs a)

instance (ZqTuple a, ZqTuple b) => ZqTuple (a, b) where
  type ModPairs (a,b) = (ModPairs a, ModPairs b)
  getModuli =
    let as = proxy getModuli (Proxy::Proxy a)
        bs = proxy getModuli (Proxy :: Proxy b)
    in tag (as,bs)

-- counts components in a nested tuple
class Tuple a where
  numComponents :: Tagged a Int16

instance {-# Overlappable #-} Tuple a where
  numComponents = tag 1

instance (Tuple a, Tuple b) => Tuple (a,b) where
  numComponents = tag $ (proxy numComponents (Proxy::Proxy a)) + (proxy numComponents (Proxy::Proxy b))

-- | Single-argument synonym for @Dispatch'@.
type Dispatch r = (Dispatch' (CTypeOf r) r)

-- | Class to safely match Haskell types with the appropriate C function.
class (repr ~ CTypeOf r) => Dispatch' repr r where
  -- | Equivalent to 'Tensor's @crt@.
  dcrt      :: Ptr (Ptr r) ->           Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @crtInv@.
  dcrtinv   :: Ptr (Ptr r) -> Ptr r ->  Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @tGaussianDec@.
  dgaussdec :: Ptr (Ptr (Complex r)) -> Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
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

instance (Tuple r, CTypeOf r ~ ComplexD) => Dispatch' ComplexD r where
  dcrt ruptr pout totm pfac numFacts =
    tensorCRTC (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts (castPtr ruptr)
  dcrtinv ruptr minv pout totm pfac numFacts =
    tensorCRTInvC (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts (castPtr ruptr) (castPtr minv)
  dl pout =
    tensorLC (proxy numComponents (Proxy::Proxy r)) (castPtr pout)
  dlinv pout =
    tensorLInvC (proxy numComponents (Proxy::Proxy r)) (castPtr pout)
  dnorm = error "cannot call CT normSq on type Complex Double"
  dmulgpow pout =
    tensorGPowC (proxy numComponents (Proxy::Proxy r)) (castPtr pout)
  dmulgdec = error "cannot call CT mulGDec on type Complex Double"
  dginvpow pout =
    tensorGInvPowC (proxy numComponents (Proxy::Proxy r)) (castPtr pout)
  dginvdec = error "cannot call CT divGDec on type Complex Double"
  dmul aout bout =
    mulC (proxy numComponents (Proxy::Proxy r)) (castPtr aout) (castPtr bout)
  dgaussdec = error "cannot call CT gaussianDec on type Comple Double"

instance (Tuple r, CTypeOf r ~ DoubleD) => Dispatch' DoubleD r where
  dcrt = error "cannot call CT Crt on type Double"
  dcrtinv = error "cannot call CT CrtInv on type Double"
  dl pout =
    tensorLDouble (proxy numComponents (Proxy::Proxy r)) (castPtr pout)
  dlinv pout =
    tensorLInvDouble (proxy numComponents (Proxy::Proxy r)) (castPtr pout)
  dnorm pout = tensorNormSqD (proxy numComponents (Proxy::Proxy r)) (castPtr pout)
  dmulgpow = error "cannot call CT mulGPow on type Double"
  dmulgdec = error "cannot call CT mulGDec on type Double"
  dginvpow = error "cannot call CT divGPow on type Double"
  dginvdec = error "cannot call CT divGDec on type Double"
  dmul = error "cannot call CT (*) on type Double"
  dgaussdec ruptr pout totm pfac numFacts =
    tensorGaussianDec (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts (castPtr ruptr)

instance (Tuple r, CTypeOf r ~ Int64D) => Dispatch' Int64D r where
  dcrt = error "cannot call CT Crt on type Int64"
  dcrtinv = error "cannot call CT CrtInv on type Int64"
  dl pout =
    tensorLR (proxy numComponents (Proxy::Proxy r)) (castPtr pout)
  dlinv pout =
    tensorLInvR (proxy numComponents (Proxy::Proxy r)) (castPtr pout)
  dnorm pout =
    tensorNormSqR (proxy numComponents (Proxy::Proxy r)) (castPtr pout)
  dmulgpow pout =
    tensorGPowR (proxy numComponents (Proxy::Proxy r)) (castPtr pout)
  dmulgdec pout =
    tensorGDecR (proxy numComponents (Proxy::Proxy r)) (castPtr pout)
  dginvpow pout =
    tensorGInvPowR (proxy numComponents (Proxy::Proxy r)) (castPtr pout)
  dginvdec pout =
    tensorGInvDecR (proxy numComponents (Proxy::Proxy r)) (castPtr pout)
  dmul = error "cannot call CT (*) on type Int64"
  dgaussdec = error "cannot call CT gaussianDec on type Int64"

foreign import ccall unsafe "tensorLR" tensorLR ::                  Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLInvR" tensorLInvR ::            Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLDouble" tensorLDouble ::       Int16 -> Ptr Double -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLInvDouble" tensorLInvDouble :: Int16 -> Ptr Double -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLC" tensorLC ::       Int16 -> Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLInvC" tensorLInvC :: Int16 -> Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()

foreign import ccall unsafe "tensorNormSqR" tensorNormSqR ::     Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorNormSqD" tensorNormSqD ::     Int16 -> Ptr Double -> Int64 -> Ptr CPP -> Int16          -> IO ()

foreign import ccall unsafe "tensorGPowR" tensorGPowR ::         Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGPowC" tensorGPowC ::         Int16 -> Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGDecR" tensorGDecR ::         Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGInvPowR" tensorGInvPowR ::   Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGInvPowC" tensorGInvPowC ::   Int16 -> Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGInvDecR" tensorGInvDecR ::   Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()

foreign import ccall unsafe "tensorCRTC" tensorCRTC ::           Int16 -> Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (Complex Double)) -> IO ()
foreign import ccall unsafe "tensorCRTInvC" tensorCRTInvC ::     Int16 -> Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (Complex Double)) -> Ptr (Complex Double) -> IO ()

foreign import ccall unsafe "tensorGaussianDec" tensorGaussianDec :: Int16 -> Ptr Double -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (Complex Double)) ->  IO ()
foreign import ccall unsafe "mulC" mulC :: Int16 -> Ptr (Complex Double) -> Ptr (Complex Double) -> Int64 -> IO ()
