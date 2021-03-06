{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Trident.Static.Tensor where

import           Data.Kind
import           Data.Proxy
import           Data.Singletons
import           Foreign.ForeignPtr
import           Foreign.Storable
import           GHC.TypeLits
import           System.Random.MWC             (UniformRange)

import           Trident.Core.Shape

data DataType = Float
              | Double

data DataFormat = NCHW | NHWC 

data Transpose = NoTranspose | Tranpose | Conj | ConjTranpose

data Backend = BLAS

-- type family ToConcreteType (dtype :: DataType) :: Type where
--   ToConcreteType 'Float  = Float
--   ToConcreteType 'Double = Double
--   ToConcreteType x       = TypeError ('Text "Datatype is not supported")

class Storable dtype => CreatableTensor  (backend :: Backend) (dtype :: Type) where
  data Tensor (backend :: Backend) (dtype :: Type) (shape :: Shape)

  -- * functions to create a tensor

  fromList :: ( KnownNat s, s ~ ShapeSize shape ) => [dtype] -> Tensor backend dtype shape

  toList :: Tensor backend dtype shape -> [dtype]

  build :: ( KnownNat s, s ~ ShapeSize shape, SingI shape ) => (Index -> dtype) -> Tensor backend dtype shape

class CreatableTensor backend dtype => RandomTensor (backend :: Backend) (dtype :: Type) where
  randomTensor :: ( KnownNat s, s ~ ShapeSize shape, UniformRange dtype ) => (dtype, dtype) -> IO (Tensor backend dtype shape)

-- todo: remove shape from type class, probably by creating some singleton function that converts indexes to
-- linear indexes ?
class CreatableTensor backend dtype => IndexableTensor (backend :: Backend) (dtype :: Type) (shape :: Shape) where
  -- | type level indexing using singleton, will catch out of bounds error at compile time
  typedIndex :: ( IsInRange index shape ) => Tensor backend dtype shape -> Idx index -> dtype

  -- | term level indexing to prevent having to unsafe coerce the constraint for typedIndex, although
  --   if the index is known at compile time, this is less safe.
  index :: Tensor backend dtype shape -> Index -> dtype

-- | a nicer syntax for indexing
(!) :: IndexableTensor backend dtype shape => Tensor backend dtype shape -> Index -> dtype
(!) = index

-- todo: make the src and destination datatypes different, they dont have to be the same. Although this
-- type class def probably wont work
class CreatableTensor backend dtype => TraversableTensor backend dtype where
  mapTensor :: (dtype -> dtype) -> Tensor backend dtype shape -> Tensor backend dtype shape

  zipTensors :: (dtype -> dtype -> dtype) -> Tensor backend dtype shape -> Tensor backend dtype shape -> Tensor backend dtype shape

  foldTensor :: (a -> dtype -> a) -> a -> Tensor backend dtype shape-> a

class ( TraversableTensor backend dtype, Num dtype ) => MathTensor backend dtype where
  -- performs alpha * A * B (potentially tranposing)
  gemm :: ( KnownNat a, KnownNat b, KnownNat c )
       => Tensor backend dtype ('D2 a b) -> Transpose
       -> Tensor backend dtype ('D2 b c) -> Transpose
       -> dtype
       -> Tensor backend dtype ('D2 a c)
  
  mmul :: ( KnownNat a, KnownNat b, KnownNat c )
       => Tensor backend dtype ('D2 a b) -> Tensor backend dtype ('D2 b c) -> Tensor backend dtype ('D2 a c)
  mmul m1 m2 = gemm m1 NoTranspose m2 NoTranspose 1 

  add :: Tensor backend dtype shape -> Tensor backend dtype shape -> Tensor backend dtype shape
  add = zipTensors (+)

  mul :: Tensor backend dtype shape -> Tensor backend dtype shape -> Tensor backend dtype shape
  mul = zipTensors (*)

  sumTensor :: Tensor backend dtype shape -> dtype
  sumTensor = foldTensor (+) 0
