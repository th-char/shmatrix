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
module Numeric.Static.Tensor where

import           Data.Kind
import           Data.Proxy
import           Data.Singletons
import           Foreign.ForeignPtr
import           Foreign.Storable
import           GHC.TypeLits

import           Numeric.Static.Internal.Shape

data DataType = Float
              | Double

data DataFormat = NCHW

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

-- todo: remove shape from type class
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

class CreatableTensor backend dtype => MathTensor backend dtype where 
  mmul2d :: ( KnownNat a, KnownNat b, KnownNat c ) 
         => Tensor backend dtype ('D2 a b) -> Tensor backend dtype ('D2 b c) -> Tensor backend dtype ('D2 a c)

  add :: Tensor backend dtype shape -> Tensor backend dtype shape -> Tensor backend dtype shape

  mul :: Tensor backend dtype shape -> Tensor backend dtype shape -> Tensor backend dtype shape

  sumTensor :: Tensor backend dtype shape -> dtype
