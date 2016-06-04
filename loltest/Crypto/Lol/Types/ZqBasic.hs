{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, NoImplicitPrelude, PolyKinds,
             RebindableSyntax, RoleAnnotations, ScopedTypeVariables,
             TypeFamilies, UndecidableInstances #-}

-- | An implementation of the quotient ring Zq = Z/qZ.

-- EAC: It may help GHC do specialization at higher levels of the library
-- if we "simplify" constraints in this module. For example, replace the
-- (Additive (ZqBasic q z)) constraint on the Reduce instance with
-- (Additive z)

module Crypto.Lol.Types.ZqBasic
( ZqBasic -- export the type, but not the constructor (for safety)
) where

import Crypto.Lol.CRTrans
import Crypto.Lol.Prelude           as LP
import Crypto.Lol.Reflects

import Math.NumberTheory.Primes.Factorisation
import Math.NumberTheory.Primes.Testing

import Control.Applicative
import Control.Arrow
import Control.DeepSeq        (NFData)
import Data.Coerce
import Data.Maybe
import NumericPrelude.Numeric as NP (round)
import System.Random
import Test.QuickCheck

-- for the Unbox instances
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U

import Foreign.Storable

import qualified Algebra.Additive       as Additive (C)
import qualified Algebra.Field          as Field (C)
import qualified Algebra.IntegralDomain as IntegralDomain (C)
import qualified Algebra.Ring           as Ring (C)
import qualified Algebra.ZeroTestable   as ZeroTestable (C)

-- | The ring @Z_q@ of integers modulo 'q', using underlying integer
-- type 'z'.
newtype ZqBasic q z = ZqB z
    deriving (Eq, Ord, ZeroTestable.C, Show, NFData, Storable)

-- the q argument, though phantom, matters for safety
type role ZqBasic nominal representational

--deriving instance (U.Unbox i) => G.Vector U.Vector (ZqBasic q i)
--deriving instance (U.Unbox i) => M.MVector U.MVector (ZqBasic q i)
--deriving instance (U.Unbox i) => U.Unbox (ZqBasic q i)

{-# INLINABLE reduce' #-}
reduce' :: forall q z . (Reflects q z, ToInteger z) => z -> ZqBasic q z
reduce' = ZqB . (`mod` proxy value (Proxy::Proxy q))

-- puts value in range [-q/2, q/2)
decode' :: forall q z . (Reflects q z, ToInteger z) => ZqBasic q z -> z
decode' = let qval = proxy value (Proxy::Proxy q)
          in \(ZqB x) -> if 2 * x < qval then x else x - qval

instance (Reflects q z, ToInteger z, Enum z) => Enumerable (ZqBasic q z) where
  values = let qval :: z = proxy value (Proxy::Proxy q)
           in coerce [0..(qval-1)]

instance (Reflects q z, ToInteger z) => Mod (ZqBasic q z) where
  type ModRep (ZqBasic q z) = z

  modulus = retag (value :: Tagged q z)

instance (Reflects q z, ToInteger z) => Reduce z (ZqBasic q z) where
  reduce = reduce'

instance (Reflects q z, ToInteger z, Additive (ZqBasic q z)) => Reduce Integer (ZqBasic q z) where
  reduce = fromInteger

type instance LiftOf (ZqBasic q z) = z

instance (Reflects q z, ToInteger z) => Lift' (ZqBasic q z) where
  lift = decode'

instance (Reflects q z, ToInteger z, Reflects q' z, Ring z)
         => Rescale (ZqBasic q z) (ZqBasic q' z) where

  rescale = rescaleMod

instance (Reflects p z, Reflects q z, ToInteger z, Field (ZqBasic q z), Field (ZqBasic p z))
         => Encode (ZqBasic p z) (ZqBasic q z) where

  lsdToMSD = let pval :: z = proxy value (Proxy::Proxy p)
                 negqval :: z = negate $ proxy value (Proxy::Proxy q)
             in (reduce' negqval, recip $ reduce' pval)

-- | Yield a /principal/ @m@th root of unity @omega_m \in @Z_q^*@.
-- The implementation requires @q@ to be prime.  It works by finding a
-- generator of @Z_q^*@ and raising it to the @(q-1)/m@ power.
-- Therefore, outputs for different values of @m@ are consistent,
-- i.e., @omega_{m'}^(m'/m) = omega_m@.
principalRootUnity ::
    forall m q z . (Reflects m Int, Reflects q z, ToInteger z, Enumerable (ZqBasic q z))
               => TaggedT m Maybe (Int -> ZqBasic q z)
principalRootUnity =        -- use Integers for all intermediate calcs
  let qval = fromIntegral $ (proxy value (Proxy::Proxy q) :: z)
      mval = proxy value (Proxy::Proxy m)
      -- order of Zq^* (assuming q prime)
      order = qval-1
      -- the primes dividing the order of Zq^*
      pfactors = fst <$> factorise order
      -- the powers we need to check
      exps = div order <$> pfactors
      -- whether an element is a generator of Zq^*
      isGen x = (x^order == one) && all (\e -> x^e /= one) exps
  in tagT $ if isPrime qval -- for simplicity, require q to be prime
            then let (mq,mr) = order `divMod` fromIntegral mval
                 in if mr == 0
                    then let omega = head (filter isGen values) ^ mq
                             omegaPows = V.iterateN mval (*omega) one
                         in Just $ (omegaPows V.!) . (`mod` mval)
                    else Nothing
            else Nothing       -- fail if q composite

mhatInv :: forall m q z . (Reflects m Int, Reflects q z, ToInteger z, PID z)
           => TaggedT m Maybe (ZqBasic q z)
mhatInv = let qval = proxy value (Proxy::Proxy q)
          in peelT $ (fmap reduce' . (`modinv` qval) . fromIntegral) <$>
                 valueHat <$> (value :: Tagged m Int)

-- instance of CRTrans
instance (Reflects q z, ToInteger z, PID z, Enumerable (ZqBasic q z))
         => CRTrans Maybe (ZqBasic q z) where

  crtInfo = (,) <$> principalRootUnity <*> mhatInv

-- instance of CRTEmbed
instance (Reflects q z, ToInteger z, Ring (ZqBasic q z)) => CRTEmbed (ZqBasic q z) where
  type CRTExt (ZqBasic q z) = Complex Double

  toExt (ZqB x) = fromReal $ fromIntegral x
  fromExt x = reduce' $ NP.round $ real x

-- instance of Additive
instance (Reflects q z, ToInteger z, Additive z) => Additive.C (ZqBasic q z) where

  {-# INLINABLE zero #-}
  zero = ZqB zero

  {-# INLINABLE (+) #-}
  (+) = let qval = proxy value (Proxy::Proxy q)
        in \ (ZqB x) (ZqB y) ->
        let z = x + y
        in ZqB (if z >= qval then z - qval else z)

  {-# INLINABLE negate #-}
  negate (ZqB x) = reduce' $ negate x

-- instance of Ring
instance (Reflects q z, ToInteger z, Ring z) => Ring.C (ZqBasic q z) where
  {-# INLINABLE (*) #-}
  (ZqB x) * (ZqB y) = reduce' $ x * y

  {-# INLINABLE fromInteger #-}
  fromInteger =
    let qval = toInteger (proxy value (Proxy::Proxy q) :: z)
    -- this is safe as long as type z can hold the value of q
    in \x -> ZqB $ fromInteger $ x `mod` qval

-- instance of Field
instance (Reflects q z, ToInteger z, PID z, Show z) => Field.C (ZqBasic q z) where

  {-# INLINABLE recip #-}
  recip = let qval = proxy value (Proxy::Proxy q)
              -- safe because modinv returns in range 0..qval-1
          in \(ZqB x) -> ZqB $
               fromMaybe (error $ "ZqB.recip fail: " ++
                         show x ++ "\t" ++ show qval) $ modinv x qval

-- (canonical) instance of IntegralDomain, needed for Cyclotomics
instance (Reflects q z, ToInteger z, Field (ZqBasic q z)) => IntegralDomain.C (ZqBasic q z) where
  divMod a b = (a/b, zero)

-- instance of Random
instance (Reflects q z, ToInteger z, Random z) => Random (ZqBasic q z) where
  random = let high = proxy value (Proxy::Proxy q) - 1
           in \g -> let (x,g') = randomR (0,high) g
                    in (ZqB x, g')

  randomR _ = error "randomR non-sensical for Zq types"

-- instance of Arbitrary
instance (Reflects q z, ToInteger z, Random z) => Arbitrary (ZqBasic q z) where
  arbitrary =
    let qval :: z = proxy value (Proxy::Proxy q)
    in fromIntegral <$> choose (0, qval-1)

  shrink = shrinkNothing

-- CJP: restored manual Unbox instances, until we have a better way
-- (NewtypeDeriving or TH)

newtype instance U.MVector s (ZqBasic q z) = MV_ZqBasic (U.MVector s z)
newtype instance U.Vector (ZqBasic q z) = V_ZqBasic (U.Vector z)

-- Unbox, when underlying representation is
instance U.Unbox z => U.Unbox (ZqBasic q z)

{- purloined and tweaked from code in `vector` package that defines
types as unboxed -}
instance U.Unbox z => M.MVector U.MVector (ZqBasic q z) where
  basicLength (MV_ZqBasic v) = M.basicLength v
  basicUnsafeSlice z n (MV_ZqBasic v) = MV_ZqBasic $ M.basicUnsafeSlice z n v
  basicOverlaps (MV_ZqBasic v1) (MV_ZqBasic v2) = M.basicOverlaps v1 v2
  basicInitialize (MV_ZqBasic v) = M.basicInitialize v
  basicUnsafeNew n = MV_ZqBasic <$> M.basicUnsafeNew n
  basicUnsafeReplicate n (ZqB x) = MV_ZqBasic <$> M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_ZqBasic v) z = ZqB <$> M.basicUnsafeRead v z
  basicUnsafeWrite (MV_ZqBasic v) z (ZqB x) = M.basicUnsafeWrite v z x
  basicClear (MV_ZqBasic v) = M.basicClear v
  basicSet (MV_ZqBasic v) (ZqB x) = M.basicSet v x
  basicUnsafeCopy (MV_ZqBasic v1) (MV_ZqBasic v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_ZqBasic v1) (MV_ZqBasic v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_ZqBasic v) n = MV_ZqBasic <$> M.basicUnsafeGrow v n

instance U.Unbox z => G.Vector U.Vector (ZqBasic q z) where
  basicUnsafeFreeze (MV_ZqBasic v) = V_ZqBasic <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_ZqBasic v) = MV_ZqBasic <$> G.basicUnsafeThaw v
  basicLength (V_ZqBasic v) = G.basicLength v
  basicUnsafeSlice z n (V_ZqBasic v) = V_ZqBasic $ G.basicUnsafeSlice z n v
  basicUnsafeIndexM (V_ZqBasic v) z = ZqB <$> G.basicUnsafeIndexM v z
  basicUnsafeCopy (MV_ZqBasic mv) (V_ZqBasic v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
