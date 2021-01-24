module Numeric.Static.BLAS.Internal.OpenBLAS where

-- see: https://developer.apple.com/documentation/accelerate/1513264-cblas_sgemm
type GemmFFI dtype
  =    CBLAS_ORDERT      -- order
    -> CBLAS_TRANSPOSET  -- transA
    -> CBLAS_TRANSPOSET  -- transB
    -> CInt              -- m
    -> CInt              -- n
    -> CInt              -- k
    -> dtype             -- alpha
    -> Ptr dtype         -- A
    -> CInt              -- lda
    -> Ptr dtype         -- B
    -> CInt              -- ldb
    -> dtype             -- beta
    -> Ptr dtype         -- C
    -> CInt              -- ldc
    -> IO ()

foreign import ccall unsafe "cblas_sgemm"
  cblas_sgemm_unsafe :: GemmFFI Float

foreign import ccall unsafe "cblas_dgemm"
  cblas_dgemm_unsafe :: GemmFFI Double
