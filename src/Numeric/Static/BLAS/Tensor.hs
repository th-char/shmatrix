{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Numeric.Static.BLAS.Tensor where

import           Data.Kind
import           Data.Proxy
import           Data.Singletons
import           Foreign.ForeignPtr
import           Foreign.Storable
import           GHC.TypeLits
import           System.IO.Unsafe

import           Numeric.Static.Internal.Memory
import           Numeric.Static.Internal.Shape
import           Numeric.Static.Tensor

-- data BLAS = BLAS

instance ( Storable dtype )
  => CreatableTensor 'BLAS dtype where

  data Tensor 'BLAS dtype shape
    = UnsafeMkBLASTensor {-# UNPACK #-} !Int
                         {-# UNPACK #-} !DataFormat
                         {-# UNPACK #-} !(ForeignPtr dtype)

  -- TODO: need to work out where to do the shape check, is it worth possible space leak ?
  fromList :: forall s shape. ( KnownNat s, s ~ ShapeSize shape ) => [dtype] -> Tensor 'BLAS dtype shape
  fromList xs =
    let !n = fromIntegral $ natVal (Proxy :: Proxy s)
    in  unsafePerformIO $ do
          ptr <- allocatePtr n
          withForeignPtr ptr $ \ptr' -> do
            let go  _ []      = return ()
                go !i (!y:ys) = pokeElemOff ptr' i y >> go (i + 1) ys
            go 0 xs
            return $ UnsafeMkBLASTensor n NCHW ptr
  
  toList (UnsafeMkBLASTensor n NCHW ptr) =
    unsafePerformIO $ do 
      withForeignPtr ptr $ \ptr' -> do
        let go 0 _ dys = return (dys [])  
            go k i dys = do y <- peekElemOff ptr' i
                            go (k - 1) (i + 1) ( dys . (y:) )
        go n 0 id

  build :: forall s shape. ( KnownNat s, s ~ ShapeSize shape, SingI shape ) => (Index -> dtype) -> Tensor 'BLAS dtype shape
  build f =
    let !n    = fromIntegral $ natVal (Proxy :: Proxy s)
        !idxs = enumerateIdx (sing :: SShape shape)
    in  unsafePerformIO $ do
          ptr <- allocatePtr n
          withForeignPtr ptr $ \ptr' -> do
            let go  _ []      = return ()
                go !i (!y:ys) = let !z = f y in pokeElemOff ptr' i z >> go (i + 1) ys
            go 0 idxs
            return $ UnsafeMkBLASTensor n NCHW ptr

unsafeLinearIndex :: Storable dtype => ForeignPtr dtype -> Int -> dtype
unsafeLinearIndex ptr i = unsafePerformIO . withForeignPtr ptr $ \ptr' -> peekElemOff ptr' i

instance ( KnownNat a, Storable dtype )
  => IndexableTensor 'BLAS dtype ('D1 a) where

  index (UnsafeMkBLASTensor n format ptr) (Idx1 !i) =
    let !a = proxyToInt (Proxy :: Proxy a)
    in  if inRange i a
        then unsafeLinearIndex ptr i
        else error "Index out of range"

  typedIndex (UnsafeMkBLASTensor n format ptr) s =
    let Idx1 !i = singToIndex s
    in  unsafeLinearIndex ptr i

instance ( KnownNat a, KnownNat b, Storable dtype )
  => IndexableTensor 'BLAS dtype ('D2 a b) where

  index (UnsafeMkBLASTensor n format ptr) (Idx2 !i !j) =
    let !dim1 = proxyToInt (Proxy :: Proxy a)
        !dim2 = proxyToInt (Proxy :: Proxy b)
        i'    = linearIdx2 dim2 i j
    in  if inRange i dim1 && inRange j dim2
        then unsafeLinearIndex ptr i'
        else error "Index out of range"

  typedIndex (UnsafeMkBLASTensor n format ptr) s =
    let Idx2 !x !y = singToIndex s
        dim1       = proxyToInt (Proxy :: Proxy a)
        i          = linearIdx2 dim1 x y
    in  unsafeLinearIndex ptr i

instance ( KnownNat a, KnownNat b, KnownNat c, Storable dtype )
  => IndexableTensor 'BLAS dtype ('D3 a b c) where

  index (UnsafeMkBLASTensor n format ptr) (Idx3 !i !j !k) =
    let !dim1 = proxyToInt (Proxy :: Proxy a)
        !dim2 = proxyToInt (Proxy :: Proxy b)
        !dim3 = proxyToInt (Proxy :: Proxy c)
        i' = linearIdx3 dim2 dim3 i j k
    in  if inRange i dim1 && inRange j dim2 && inRange k dim3
        then unsafeLinearIndex ptr i'
        else error "Index out of range"

  typedIndex (UnsafeMkBLASTensor n format ptr) s =
    let Idx3 !x !y !z = singToIndex s
        dim1          = proxyToInt (Proxy :: Proxy a)
        dim2          = proxyToInt (Proxy :: Proxy b)
        i             = linearIdx3 dim1 dim2 x y z
    in  unsafeLinearIndex ptr i

instance ( KnownNat a, KnownNat b, KnownNat c, KnownNat d, Storable dtype )
  => IndexableTensor 'BLAS dtype ('D4 a b c d) where

  index (UnsafeMkBLASTensor n format ptr) (Idx4 !i !j !k !l) =
    let !dim1 = proxyToInt (Proxy :: Proxy a)
        !dim2 = proxyToInt (Proxy :: Proxy b)
        !dim3 = proxyToInt (Proxy :: Proxy c)
        !dim4 = proxyToInt (Proxy :: Proxy d)
        i' = linearIdx4 dim2 dim3 dim4 i j k l
    in  if inRange i dim1 && inRange j dim2 && inRange k dim3 && inRange l dim4
        then unsafeLinearIndex ptr i'
        else error "Index out of range"

  typedIndex (UnsafeMkBLASTensor n format ptr) s =
    let Idx4 !w !x !y !z = singToIndex s
        dim1             = proxyToInt (Proxy :: Proxy a)
        dim2             = proxyToInt (Proxy :: Proxy b)
        dim3             = proxyToInt (Proxy :: Proxy c)
        i                = linearIdx4 dim1 dim2 dim3 w x y z
    in  unsafeLinearIndex ptr i

instance ( Show dtype ) => Show (Tensor 'BLAS dtype shape) where
  show (UnsafeMkBLASTensor n format ptr) = "Tensor of size " ++ show n ++ show ptr

instance Storable dtype => TraversableTensor 'BLAS dtype where
  mapTensor f (UnsafeMkBLASTensor n format ptr) =
    unsafePerformIO $ do
      dstPtr <- allocatePtr n
      withForeignPtr ptr $ \ptr' ->
        withForeignPtr dstPtr $ \dstPtr' -> do
          let go 0  _  = return ()
              go !k !i = do x <- peekElemOff ptr' i
                            pokeElemOff dstPtr' i (f x)
                            go (k - 1) (i + 1)
          go n 0
      return $ UnsafeMkBLASTensor n NCHW dstPtr

  -- Can assume the sizes are the same since it type checks
  -- pls dont unsafeCoerce tensor shapes
  -- todo: should i check the sizes match ?
  zipTensors f (UnsafeMkBLASTensor n NCHW ptr1) (UnsafeMkBLASTensor _ NCHW ptr2) =
    unsafePerformIO $ do
      dstPtr <- allocatePtr n
      withForeignPtr ptr1 $ \ptr1' ->
        withForeignPtr ptr2$ \ptr2' ->
          withForeignPtr dstPtr $ \dstPtr' -> do
            let go 0  _  = return ()
                go !k !i = do x <- peekElemOff ptr1' i
                              y <- peekElemOff ptr2' i
                              pokeElemOff dstPtr' i (f x y)
                              go (k - 1) (i + 1)
            go n 0
      return $ UnsafeMkBLASTensor n NCHW dstPtr
