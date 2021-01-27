{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Utils.Comparable where

import           Hedgehog
import           Hedgehog.Internal.Source

class (Show a, Show b) => Comparable a b where
  isSimilarTo' :: a -> b -> Bool

  isSimilarTo :: (MonadTest m, HasCallStack) => a -> b -> m ()
  isSimilarTo x y =   
    withFrozenCallStack $
        diff x isSimilarTo' y

instance Comparable [Double] [Double] where
  isSimilarTo' = isSimilarListTo 1e-14

isSimilarListTo :: (Ord a, Num a) => a -> [a] -> [a] -> Bool
isSimilarListTo p = (and .) . zipWith (\a b -> a - b < p) 
