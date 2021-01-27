{-# LANGUAGE FlexibleContexts #-}
module Test.Utils.Hedgehog where

import           Hedgehog
import qualified Hedgehog.Gen             as Gen
import           Hedgehog.Internal.Source
import qualified Hedgehog.Range           as Range

genFloatList :: Int -> Gen [Float]
genFloatList n = Gen.list (Range.singleton n) $ Gen.float (Range.constantFrom 0 (-1) 1)

genDoubleList :: Int -> Gen [Double]
genDoubleList n = Gen.list (Range.singleton n) $ Gen.double (Range.constantFrom 0 (-1) 1)

blindForAll :: (Monad m, HasCallStack) => Gen a -> PropertyT m a
blindForAll = forAllWith (const "blind")