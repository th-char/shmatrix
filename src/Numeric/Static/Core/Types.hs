module Numeric.Static.Core.Types where 

type family Product (xs :: [Nat]) :: Nat where 
  Product '[]       = 1
  Product (x ': xs) = x * Product xs