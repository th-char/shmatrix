{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Numeric.Static.Internal.Shape where

import           Data.Constraint
import           Data.Kind
import           Data.Singletons
import           Data.Singletons.TypeLits hiding (natVal)
import           GHC.TypeLits             hiding (natVal)
import           GHC.TypeNats             (natVal)

data Shape where
  D1 :: Nat -> Shape
  D2 :: Nat -> Nat -> Shape
  D3 :: Nat -> Nat -> Nat -> Shape
  D4 :: Nat -> Nat -> Nat -> Nat -> Shape

data Index where 
  Idx1 :: {-# UNPACK #-} !Int -> Index
  Idx2 :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> Index
  Idx3 :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> Index
  Idx4 :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> Index

-- Singleton instances

type instance Sing = SShape

data SShape :: Shape -> Type where
  D1Sing :: Sing a -> SShape ('D1 a)
  D2Sing :: Sing a -> Sing b -> SShape ('D2 a b)
  D3Sing :: Sing a -> Sing b -> Sing c -> SShape ('D3 a b c)
  D4Sing :: Sing a -> Sing b -> Sing c -> Sing d -> SShape ('D4 a b c d)

instance KnownNat a => SingI ('D1 a) where
  sing = D1Sing sing
instance (KnownNat a, KnownNat b) => SingI ('D2 a b) where
  sing = D2Sing sing sing
instance (KnownNat a, KnownNat b, KnownNat c) => SingI ('D3 a b c) where
  sing = D3Sing sing sing sing
instance (KnownNat a, KnownNat b, KnownNat c, KnownNat d) => SingI ('D4 a b c d) where
  sing = D4Sing sing sing sing sing

singToIndex :: SShape shape -> Index
singToIndex s = case s of 
    D1Sing a@SNat                      -> Idx1 (snatToInt a)
    D2Sing a@SNat b@SNat               -> Idx2 (snatToInt a) (snatToInt b)
    D3Sing a@SNat b@SNat c@SNat        -> Idx3 (snatToInt a) (snatToInt b) (snatToInt c)
    D4Sing a@SNat b@SNat c@SNat d@SNat -> Idx4 (snatToInt a) (snatToInt b) (snatToInt c) (snatToInt d)
  where
    snatToInt :: forall n. KnownNat n => SNat n -> Int
    snatToInt SNat = fromIntegral . natVal $ (Proxy :: Proxy n)


-- Index types

type Idx = SShape

idx1 :: SingI ('D1 a) => Sing ('D1 a)
idx1 = sing

idx2 :: SingI ('D2 a b) => Sing ('D2 a b)
idx2 = sing

idx3 :: SingI ('D3 a b c) => Sing ('D3 a b c)
idx3 = sing

-- Lots of type families

type family KnownShape (shape :: Shape) :: Constraint where
  KnownShape ('D1 a)       = ( KnownNat a )
  KnownShape ('D2 a b)     = ( KnownNat a, KnownNat b )
  KnownShape ('D3 a b c)   = ( KnownNat a, KnownNat b, KnownNat c )
  KnownShape ('D4 a b c d) = ( KnownNat a, KnownNat b, KnownNat c, KnownNat d )

type family ShapeSize (shape :: Shape) :: Nat where
  ShapeSize ('D1 a)       = a
  ShapeSize ('D2 a b)     = a * b
  ShapeSize ('D3 a b c)   = a * b * c
  ShapeSize ('D4 a b c d) = a * b * c * d

class IndexableShape (shape :: Shape) (max :: Shape) where
  type IsInRange shape max :: Constraint

instance IndexableShape ('D1 a) ('D1 w) where
  type IsInRange ('D1 a) ('D1 w) = ( 0 <= a, a + 1 <= w )

instance IndexableShape ('D2 a b) ('D2 w x) where
  type IsInRange ('D2 a b) ('D2 w x) = ( 0 <= a, a + 1 <= w, 0 <= b, b + 1 <= x )

instance IndexableShape ('D3 a b c) ('D3 w x y) where
  type IsInRange ('D3 a b c) ('D3 w x y) = ( 0 <= a, a + 1 <= w, 0 <= b, b + 1 <= x,  0 <= c, c + 1 <= y )

instance IndexableShape ('D4 a b c d) ('D4 w x y z) where
  type IsInRange ('D4 a b c d) ('D4 w x y z) = ( 0 <= a, a + 1 <= w, 0 <= b, b + 1 <= x,  0 <= c, c + 1 <= y, 0 <= d, d + 1 <= z )
