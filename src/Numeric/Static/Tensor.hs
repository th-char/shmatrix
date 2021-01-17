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
import           Foreign.ForeignPtr
import           Foreign.Storable
import           GHC.TypeLits

import           Numeric.Static.Internal.Shape

data DataType = Float
              | Double

data DataFormat = NHWC

data Backend = BLAS

data Tensor (backend :: Backend) (dtype :: DataType) (shape :: Shape) where
  UnsafeMkTensor :: ( d ~ ToConcreteType dtype
                    , Storable d
                    , KnownShape shape )
                 => {-# UNPACK #-} !Int
                 -> {-# UNPACK #-} !DataFormat
                 -> {-# UNPACK #-} !(ForeignPtr d)
                 -> Tensor backend dtype shape

type family ToConcreteType (dtype :: DataType) :: Type where
  ToConcreteType 'Float  = Float
  ToConcreteType 'Double = Double
  ToConcreteType x       = TypeError ('Text "Datatype is not supported")

class CreatableTensor (backend :: Backend) (dtype :: DataType) (shape :: Shape) where
  fromList :: [ToConcreteType dtype] -> Tensor backend dtype shape

class IndexableTensor (backend :: Backend) (dtype :: DataType) (shape :: Shape) (index :: Shape) where
  atIndex :: IsInRange index shape => Tensor backend dtype shape -> Proxy index -> ToConcreteType dtype
