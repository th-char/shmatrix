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
import           GHC.TypeLits

data Shape where
  D1 :: {-# UNPACK #-} !Nat -> Shape
  D2 :: {-# UNPACK #-} !Nat -> {-# UNPACK #-} !Nat -> Shape
  D3 :: {-# UNPACK #-} !Nat -> {-# UNPACK #-} !Nat -> {-# UNPACK #-} !Nat -> Shape
  D4 :: {-# UNPACK #-} !Nat -> {-# UNPACK #-} !Nat -> {-# UNPACK #-} !Nat -> {-# UNPACK #-} !Nat -> Shape

-- Singleton instances

type instance Sing = SShape

data SShape :: Shape -> Type where
  D1Sing :: Sing a -> SShape ('D1 a)
  D2Sing :: Sing a -> Sing b -> SShape ('D2 a b)
  D3Sing :: Sing a -> Sing b -> Sing c -> SShape ('D3 a b c)

instance KnownNat a => SingI ('D1 a) where
  sing = D1Sing sing
instance (KnownNat a, KnownNat b) => SingI ('D2 a b) where
  sing = D2Sing sing sing
instance (KnownNat a, KnownNat b, KnownNat c, KnownNat (a * c)) => SingI ('D3 a b c) where
  sing = D3Sing sing sing sing

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
