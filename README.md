# shmatrix
Statically Typed Matrix Library in Haskell

Very very young library for matrix operations in Haskell, with support for nd arrays. 
There are a few things I've noticed about current matrix libraries in Haskell that I want to try and fix:
1. No real support for statically typed matrices - hmatrix has statically typed matrices, but they are just a wrapper around the 
normal matrices and have no real functionality of their own
2. Performing maps on vectors in incredibly slow, I want to try and be able to map over a vector with performance similar to that of C (I think 
there will be issues considering the lack of support for SIMD)
3. Cannot switch out the backend for matrix operations easily. I plan to add support for both CPU and GPU backends
