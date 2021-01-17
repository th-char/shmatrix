{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
module Numeric.Static.BLAS.Tensor where

import           Data.Kind
import           Data.Proxy
import           Foreign.ForeignPtr
import           Foreign.Storable
import           GHC.TypeLits
import           System.IO.Unsafe

import           Numeric.Static.Internal.Shape
import           Numeric.Static.Tensor
import           Numeric.Static.Internal.Memory

instance ( KnownNat s, KnownShape shape, s ~ ShapeSize shape, Storable (ToConcreteType dtype) ) 
  => CreatableTensor 'BLAS dtype shape where
  -- TODO: need to work out where to do the shape check, is it worth possible space leak ?
  fromList xs =
    let !n = fromIntegral $ natVal (Proxy :: Proxy s)
    in  unsafePerformIO $ do
          ptr <- allocatePtr n
          withForeignPtr ptr $ \ptr' -> do
            let go  _ []      = return ()
                go !i (!y:ys) = pokeElemOff ptr' i y >> go (i + 1) ys
            go 0 xs
            return $ UnsafeMkTensor n NHWC ptr

instance ( KnownShape ('D1 a), KnownShape ('D1 i) ) => IndexableTensor 'BLAS dtype ('D1 a) ('D1 i) where
  atIndex (UnsafeMkTensor n format ptr) Proxy =
    let !x = fromIntegral $ natVal (Proxy :: Proxy i)
    in  unsafePerformIO . withForeignPtr ptr $ \ptr' -> peekElemOff ptr' x

instance ( Show (ToConcreteType dtype) ) => Show (Tensor 'BLAS dtype shape) where
  show (UnsafeMkTensor n format ptr) = "Tensor of size " ++ show n ++ show ptr
