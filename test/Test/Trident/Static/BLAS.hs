{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Test.Trident.Static.BLAS where

import           Data.Proxy
import           GHC.TypeLits

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import qualified Numeric.LinearAlgebra      as H

import           Trident.Core.Shape
import           Trident.Static.BLAS
import           Trident.Static.Tensor

import           Test.Utils.Hedgehog
import           Test.Utils.Comparable


prop_can_create_tensor_from_list_of_floats :: Property
prop_can_create_tensor_from_list_of_floats = property $ do
  l  <- forAll $ Gen.int (Range.constant 5 100)
  xs <- forAll $ genFloatList l

  case someNatVal $ fromIntegral l of
    Just (SomeNat (_ :: Proxy length)) -> do
      let t = fromList xs :: Tensor 'BLAS Float ('D1 length)
      t `seq` success

prop_tensor_list_roundtrip :: Property
prop_tensor_list_roundtrip = property $ do
  l  <- forAll $ Gen.int (Range.constant 5 100)
  xs <- forAll $ genFloatList l

  case someNatVal $ fromIntegral l of
    Just (SomeNat (_ :: Proxy length)) -> do
      let t  = fromList xs :: Tensor 'BLAS Float ('D1 length)
          ys = toList t
      xs === ys

prop_can_index_1d_float_tensor :: Property
prop_can_index_1d_float_tensor = property $ do
  w  <- forAll $ Gen.int (Range.constant 5 100)
  xs <- forAll $ genFloatList w

  case someNatVal $ fromIntegral w of
    Just (SomeNat (_ :: Proxy w)) -> do
      let t  = fromList xs :: Tensor 'BLAS Float ('D1 w)
          ys = ( (flip map) [0..w-1] $ \i ->
            t ! Idx1 i )
      xs === ys

prop_can_index_2d_float_tensor :: Property
prop_can_index_2d_float_tensor = property $ do
  w  <- forAll $ Gen.int (Range.constant 1 5)
  h  <- forAll $ Gen.int (Range.constant 1 5)
  let n = w * h
  xs <- forAll $ genFloatList n

  case (someNatVal $ fromIntegral w, someNatVal $ fromIntegral h) of
    (Just (SomeNat (_ :: Proxy w)), Just (SomeNat (_ :: Proxy h))) -> do
      let t  = fromList xs :: Tensor 'BLAS Float ('D2 h w)
          ys = [ t ! Idx2 i j | i <- [0..h-1], j <- [0..w-1] ]
      xs === ys

prop_can_index_3d_float_tensor :: Property
prop_can_index_3d_float_tensor = property $ do
  w  <- forAll $ Gen.int (Range.constant 1 4)
  h  <- forAll $ Gen.int (Range.constant 1 4)
  c  <- forAll $ Gen.int (Range.constant 1 4)
  let n = w * h * c
  xs <- forAll $ genFloatList n

  case (someNatVal $ fromIntegral w, someNatVal $ fromIntegral h, someNatVal $ fromIntegral c) of
    (Just (SomeNat (_ :: Proxy w)), Just (SomeNat (_ :: Proxy h)), Just (SomeNat (_ :: Proxy c))) -> do
      let t  = fromList xs :: Tensor 'BLAS Float ('D3 c h w)
          ys = [ t ! Idx3 k i j | k <- [0..c-1], i <- [0..h-1], j <- [0..w-1] ]
      xs === ys

prop_can_index_4d_float_tensor :: Property
prop_can_index_4d_float_tensor = property $ do
  w  <- forAll $ Gen.int (Range.constant 1 4)
  h  <- forAll $ Gen.int (Range.constant 1 4)
  c  <- forAll $ Gen.int (Range.constant 1 4)
  n  <- forAll $ Gen.int (Range.constant 1 4)
  let l = w * h * c * n
  xs <- forAll $ genFloatList l

  case (someNatVal $ fromIntegral w, someNatVal $ fromIntegral h, someNatVal $ fromIntegral c, someNatVal $ fromIntegral n) of
    (Just (SomeNat (_ :: Proxy w)), Just (SomeNat (_ :: Proxy h)), Just (SomeNat (_ :: Proxy c)), Just (SomeNat (_ :: Proxy n))) -> do
      let t  = fromList xs :: Tensor 'BLAS Float ('D4 n c h w)
          ys = [ t ! Idx4 m k i j | m <- [0..n-1], k <- [0..c-1], i <- [0..h-1], j <- [0..w-1] ]
      xs === ys

prop_map_tensor_same_as_map_list :: Property
prop_map_tensor_same_as_map_list = property $ do
  w  <- forAll $ Gen.int (Range.constant 1 10)
  h  <- forAll $ Gen.int (Range.constant 1 10)
  let l = h * w
  xs <- forAll $ genFloatList l
  f  <- blindForAll $ Gen.element [(+ 0.5), (\x -> x - 0.5), (* 2), (/ 2)]

  let refys = map f xs

  case ( someNatVal $ fromIntegral w, someNatVal $ fromIntegral h ) of
    ( Just (SomeNat (_ :: Proxy w)), Just (SomeNat (_ :: Proxy h)) ) -> do
      let t  = fromList xs :: Tensor 'BLAS Float ('D2 h w)
          t' = mapTensor f t
          ys = toList t'
      refys === ys

prop_zip_tensors_same_as_zip_lists :: Property
prop_zip_tensors_same_as_zip_lists = property $ do
  w  <- forAll $ Gen.int (Range.constant 1 8)
  h  <- forAll $ Gen.int (Range.constant 1 8)
  let l = h * w
  xs1 <- forAll $ genFloatList l
  xs2 <- forAll $ genFloatList l
  f  <- blindForAll $ Gen.element [(+), (-), (*)]

  let refys = zipWith f xs1 xs2

  case ( someNatVal $ fromIntegral w, someNatVal $ fromIntegral h ) of
    ( Just (SomeNat (_ :: Proxy w)), Just (SomeNat (_ :: Proxy h)) ) -> do
      let t1  = fromList xs1 :: Tensor 'BLAS Float ('D2 h w)
          t2  = fromList xs2 :: Tensor 'BLAS Float ('D2 h w)
          t' = zipTensors f t1 t2
          ys = toList t'
      refys === ys

prop_fold_tensors_same_as_foldl_lists :: Property
prop_fold_tensors_same_as_foldl_lists = property $ do
  w  <- forAll $ Gen.int (Range.constant 1 10)
  h  <- forAll $ Gen.int (Range.constant 1 10)
  let l = h * w
  xs <- forAll $ genFloatList l
  f  <- blindForAll $ Gen.element [(+), (*)]

  let refy = foldl f 1 xs

  case ( someNatVal $ fromIntegral w, someNatVal $ fromIntegral h ) of
    ( Just (SomeNat (_ :: Proxy w)), Just (SomeNat (_ :: Proxy h)) ) -> do
      let t  = fromList xs :: Tensor 'BLAS Float ('D2 h w)
          y = foldTensor f 1 t
      refy === y

prop_multiplying_two_float_tensors_same_as_hmatrix :: Property
prop_multiplying_two_float_tensors_same_as_hmatrix = property $ do
  a  <- forAll $ Gen.int (Range.constant 1 10)
  b  <- forAll $ Gen.int (Range.constant 1 10)
  c  <- forAll $ Gen.int (Range.constant 1 10)

  xs <- forAll $ genFloatList (a * b)
  ys <- forAll $ genFloatList (b * c)

  let m1    = (a H.>< b) xs
      m2    = (b H.>< c) ys
      refzs = concat . H.toLists $ m1 H.<> m2

  case ( someNatVal $ fromIntegral a, someNatVal $ fromIntegral b, someNatVal $ fromIntegral c ) of
    ( Just (SomeNat (_ :: Proxy a)), Just (SomeNat (_ :: Proxy b)), Just (SomeNat (_ :: Proxy c)) ) -> do
      let t1  = fromList xs :: Tensor 'BLAS Float ('D2 a b)
          t2  = fromList ys :: Tensor 'BLAS Float ('D2 b c)
          t   = mmul t1 t2
          zs  = toList t

      refzs === zs

prop_multiplying_two_double_tensors_same_as_hmatrix :: Property
prop_multiplying_two_double_tensors_same_as_hmatrix = property $ do
  a  <- forAll $ Gen.int (Range.constant 1 10)
  b  <- forAll $ Gen.int (Range.constant 1 10)
  c  <- forAll $ Gen.int (Range.constant 1 10)

  xs <- forAll $ genDoubleList (a * b)
  ys <- forAll $ genDoubleList (b * c)

  let m1    = (a H.>< b) xs
      m2    = (b H.>< c) ys
      refzs = concat . H.toLists $ m1 H.<> m2

  case ( someNatVal $ fromIntegral a, someNatVal $ fromIntegral b, someNatVal $ fromIntegral c ) of
    ( Just (SomeNat (_ :: Proxy a)), Just (SomeNat (_ :: Proxy b)), Just (SomeNat (_ :: Proxy c)) ) -> do
      let t1  = fromList xs :: Tensor 'BLAS Double ('D2 a b)
          t2  = fromList ys :: Tensor 'BLAS Double ('D2 b c)
          t   = mmul t1 t2
          zs  = toList t

      refzs `isSimilarTo` zs

tests :: IO Bool
tests = checkParallel $$(discover)
