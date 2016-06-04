{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

module CTensor where

import Data.Int
import Foreign.Ptr      (Ptr, castPtr)
import Foreign.Storable (Storable (..))
import GHC.Exts

class (TElt t Double) => Tensor t where
  type TElt t r :: Constraint

  scalarPow :: (Num r, TElt t r) => r -> t r
  l, lInv :: (Num r, TElt t r) => t r -> t r

data CT r = CT [r] deriving (Show)

instance Tensor CT where
  type TElt CT r = (Dispatch r)
  scalarPow = error "SUCCESS!"

data CPP = CPP {p' :: !Int32, e' :: !Int16}
instance Storable CPP
instance Show CPP where show (CPP p e) = "(" ++ show p ++ "," ++ show e ++ ")"

data DoubleD
data Int64D

type family CTypeOf x where
  CTypeOf Double = DoubleD
  CTypeOf Int64 = Int64D

-- | Single-argument synonym for @Dispatch'@.
type Dispatch r = (Dispatch' (CTypeOf r) r)

-- | Class to safely match Haskell types with the appropriate C function.
class (repr ~ CTypeOf r) => Dispatch' repr r where
  -- | Equivalent to 'Tensor's @l@.
  dl        :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @lInv@.
  dlinv     :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()

instance (CTypeOf r ~ DoubleD) => Dispatch' DoubleD r where
  dl pout =
    tensorLDouble 1 (castPtr pout)
  dlinv pout =
    tensorLInvDouble 1 (castPtr pout)

instance (CTypeOf r ~ Int64D) => Dispatch' Int64D r where
  dl pout =
    tensorLR 1 (castPtr pout)
  dlinv pout =
    tensorLInvR 1 (castPtr pout)

foreign import ccall unsafe "tensorLR" tensorLR ::                 Int16 -> Ptr Int64  -> Int64 -> Ptr CPP -> Int16 -> IO ()
foreign import ccall unsafe "tensorLInvR" tensorLInvR ::           Int16 -> Ptr Int64  -> Int64 -> Ptr CPP -> Int16 -> IO ()
foreign import ccall unsafe "tensorLDouble" tensorLDouble ::       Int16 -> Ptr Double -> Int64 -> Ptr CPP -> Int16 -> IO ()
foreign import ccall unsafe "tensorLInvDouble" tensorLInvDouble :: Int16 -> Ptr Double -> Int64 -> Ptr CPP -> Int16 -> IO ()