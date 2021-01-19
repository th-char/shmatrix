{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Tests.Numeric.Static.BLAS where

import           Data.Constraint
import           Data.Proxy
import           GHC.TypeLits
import           Unsafe.Coerce

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Numeric.Static.BLAS.Tensor
import           Numeric.Static.Internal.Shape
import           Numeric.Static.Tensor

prop_can_create_tensor_from_list_of_floats :: Property
prop_can_create_tensor_from_list_of_floats = property $ do
  l  <- forAll $ Gen.int (Range.constant 5 100)
  xs <- forAll $ Gen.list (Range.singleton l) $ Gen.float (Range.constant 0 100)

  case someNatVal $ fromIntegral l of
    Just (SomeNat (_ :: Proxy length)) -> do
      let t = fromList xs :: Tensor 'BLAS 'Float ('D1 length)
      t `seq` success

prop_can_index_float_tensor :: Property
prop_can_index_float_tensor = property $ do
  l  <- forAll $ Gen.int (Range.constant 5 100)
  xs <- forAll $ Gen.list (Range.singleton l) $ Gen.float (Range.constant 0 100)

  case someNatVal $ fromIntegral l of
    Just (SomeNat (_ :: Proxy length)) -> do
      let t  = fromList xs :: Tensor 'BLAS 'Float ('D1 length)
          ys = ( (flip map) [0..l-1] $ \i ->
            case ( someNatVal $ fromIntegral i ) of
              Just (SomeNat (_ :: Proxy i)) ->
                case (unsafeCoerce (Dict :: Dict ((), ())) :: Dict (IsInRange ('D1 i) ('D1 length)) ) of
                  Dict -> t `atIndex` (idx1 :: Idx ('D1 i)) ) :: [Float]
      xs === ys

tests :: IO Bool
tests = checkParallel $$(discover)
