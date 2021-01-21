{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Test.Numeric.Static.BLAS where

import           Data.Proxy
import           GHC.TypeLits

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Numeric.Static.BLAS.Tensor
import           Numeric.Static.Internal.Shape
import           Numeric.Static.Tensor

import           Test.Utils.Hedgehog

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

tests :: IO Bool
tests = checkParallel $$(discover)
