{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad
import System.Random.MWC

import           Criterion
import           Criterion.Main

import qualified Numeric.LinearAlgebra as H
import           Numeric.Static

import qualified Data.Massiv.Array     as MA

random10by10Tensor :: IO (Tensor 'BLAS Float ('D2 10 10))
random10by10Tensor = randomTensor (-1, 1)

random100by100Tensor :: IO (Tensor 'BLAS Float ('D2 100 100))
random100by100Tensor = randomTensor (-1, 1)

randomMassivMatrix :: Int -> Int -> IO (MA.Array MA.P MA.Ix2 Float)
randomMassivMatrix n m = do
  gen <- createSystemRandom
  xs  <- replicateM (n * m) $ uniformRM (-1, 1) gen
  return $ MA.resize' (MA.Sz (n MA.:. m)) $ MA.fromList MA.Seq xs

main :: IO ()
main = do
  defaultMain
    [ bgroup "sum 10x10 matrix elements"
        [ env (H.rand 10 10)
            $ \ ~m -> bench "hmatrix" $ nf H.sumElements m
        , env random10by10Tensor
            $ \ ~m -> bench "shmatrix" $ nf sumTensor m
        , env (randomMassivMatrix 10 10)
            $ \ ~m -> bench "massiv" $ nf (sqrt . MA.foldlS (+) 0) m
        ]
    , bgroup "sum 100x100 matrix elements"
        [ env (H.rand 100 100)     
            $ \ ~m -> bench "hmatrix" $ nf H.sumElements m
        , env random100by100Tensor 
            $ \ ~m -> bench "shmatrix" $ nf sumTensor m
        , env (randomMassivMatrix 100 100)
            $ \ ~m -> bench "massiv" $ nf (sqrt . MA.foldlS (+) 0) m
        ]
    ]
