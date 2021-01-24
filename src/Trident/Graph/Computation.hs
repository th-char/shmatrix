module Trident.Graph.Graph where 

import Trident.Core.Tensor

data Computation (backend :: Backend) (dtype :: Type) (shape :: Shape) where 
  -- zipWith
  EltWise :: (dtype -> dtype -> dtype) 
          -> Computation backend dtype shape -> Computation backend dtype shape -> Computation backend dtype shape
  
  Map     :: (dtype -> dtype) -> Computation backend dtype shape -> Computation backend dtype shape
  
  GEMM    :: forall a b c. ('D2 a b ~ shape) 
          => Computation backend dtype ('D2 a b) -> Computation backend dtype ('D2 b c) -> Computation backend dtype shape
  
  Tensor  :: Tensor dtype shape -> Computation backend dtype shape
