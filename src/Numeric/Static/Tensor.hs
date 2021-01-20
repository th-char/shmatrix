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

data DataFormat = NHWC

data Backend = BLAS

-- type family ToConcreteType (dtype :: DataType) :: Type where
--   ToConcreteType 'Float  = Float
--   ToConcreteType 'Double = Double
--   ToConcreteType x       = TypeError ('Text "Datatype is not supported")

class CreatableTensor  (backend :: Backend) (dtype :: Type) (shape :: Shape) where
  data Tensor (backend :: Backend) (dtype :: Type) (shape :: Shape)

  -- * functions to create a tensor

  fromList :: [dtype] -> Tensor backend dtype shape

  build :: (Index -> dtype) -> Tensor backend dtype shape 

class CreatableTensor backend dtype shape => IndexableTensor (backend :: Backend) (dtype :: Type) (shape :: Shape) where 
  -- | type level indexing using singleton, will catch out of bounds error at compile time
  typedIndex :: ( IsInRange index shape ) => Tensor backend dtype shape -> Idx index -> dtype

  -- | term level indexing to prevent having to unsafe coerce the constraint for typedIndex
  index :: Tensor backend dtype shape -> Index -> dtype

(!) :: IndexableTensor backend dtype shape => Tensor backend dtype shape -> Index -> dtype
(!) = index