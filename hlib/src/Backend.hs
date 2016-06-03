{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, TypeFamilies #-}

module Backend ( Dispatch ) where

import Data.Int
import Foreign.Ptr (Ptr)

-- | C representation of a prime power.
data CPP = CPP {p' :: !Int32, e' :: !Int16}

instance Show CPP where
    show (CPP p e) = "(" ++ show p ++ "," ++ show e ++ ")"

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

instance (CTypeOf r ~ DoubleD) => Dispatch' DoubleD r where
  dl pout = tensorLDouble 1 undefined

instance (CTypeOf r ~ Int64D) => Dispatch' Int64D r where
  dl pout = tensorLR 1 undefined

foreign import ccall unsafe "tensorLR" tensorLR ::                  Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16         -> IO ()
foreign import ccall unsafe "tensorLDouble" tensorLDouble ::       Int16 -> Ptr Double -> Int64 -> Ptr CPP -> Int16          -> IO ()
