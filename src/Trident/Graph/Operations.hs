module Trident.Graph.Operations where

import Trident.Graph.Computation

(|*|) :: Computation backend dtype ('D2 a b) -> Computation backend dtype ('D2 b c) -> Computation backend dtype ('D2 a c)
c1 |*| c2 = Gemm c1 c2

liftTensor :: Tensor backend dtype shape -> Computation backend dtype shape
liftTensor t = Tensor t

eltWise :: (dtype -> dtype -> dtype) -> Computation backend dtype shape -> Computation backend dtype shape -> Computation backend dtype shape
eltWise = EltWiseOp 

(_+_) :: Computation backend dtype shape -> Computation backend dtype shape -> Computation backend dtype shape
(_+_) = EltWiseOp (+)

(_*_) :: Computation backend dtype shape -> Computation backend dtype shape -> Computation backend dtype shape
(_*_) = EltWiseOp (*)

tmap :: (dtype -> dtype) -> Computation backend dtype shape -> Computation backend dtype shape
tmap = Map