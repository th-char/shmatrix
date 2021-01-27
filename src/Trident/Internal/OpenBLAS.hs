{-# LANGUAGE BangPatterns #-}

module Trident.Internal.OpenBLAS (
    MatrixOrder(..)

  , cblas_dgemm
  , cblas_sgemm
  ) where

import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO.Unsafe

import           Trident.Core.Memory
import           Trident.Static.Tensor

-- values come from https://github.com/xianyi/OpenBLAS/blob/3628b22d49bde86c022ebd7c42eef4d9297e2bb4/cblas.h#L54
{-
typedef enum CBLAS_ORDER     {CblasRowMajor=101, CblasColMajor=102} CBLAS_ORDER;
typedef enum CBLAS_TRANSPOSE {CblasNoTrans=111, CblasTrans=112, CblasConjTrans=113, CblasConjNoTrans=114} CBLAS_TRANSPOSE;
typedef enum CBLAS_UPLO      {CblasUpper=121, CblasLower=122} CBLAS_UPLO;
typedef enum CBLAS_DIAG      {CblasNonUnit=131, CblasUnit=132} CBLAS_DIAG;
typedef enum CBLAS_SIDE      {CblasLeft=141, CblasRight=142} CBLAS_SIDE;
typedef CBLAS_ORDER CBLAS_LAYOUT;
-}

data MatrixOrder        = RowMajor | ColumnMajor

newtype CBLAS_ORDER     = CBLAS_ORDER     CInt
newtype CBLAS_TRANSPOSE = CBLAS_TRANSPOSE CInt

toCInt :: Int -> CInt
toCInt = fromIntegral

orderToCBLASEnum :: MatrixOrder -> CBLAS_ORDER
orderToCBLASEnum RowMajor    = CBLAS_ORDER 101
orderToCBLASEnum ColumnMajor = CBLAS_ORDER 102

tranposeToCBLASTranpose :: Transpose -> CBLAS_TRANSPOSE
tranposeToCBLASTranpose NoTranspose  = CBLAS_TRANSPOSE 111
tranposeToCBLASTranpose Tranpose     = CBLAS_TRANSPOSE 112
tranposeToCBLASTranpose Conj         = CBLAS_TRANSPOSE 113
tranposeToCBLASTranpose ConjTranpose = CBLAS_TRANSPOSE 114

-- see: https://developer.apple.com/documentation/accelerate/1513264-cblas_sgemm
type GemmFFI dtype
  =    CBLAS_ORDER      -- ^ order
    -> CBLAS_TRANSPOSE  -- ^ transA
    -> CBLAS_TRANSPOSE  -- ^ transB
    -> CInt             -- ^ m
    -> CInt             -- ^ n
    -> CInt             -- ^ k
    -> dtype            -- ^ alpha
    -> Ptr dtype        -- ^ A
    -> CInt             -- ^ lda
    -> Ptr dtype        -- ^ B
    -> CInt             -- ^ ldb
    -> dtype            -- ^ beta
    -> Ptr dtype        -- ^ C
    -> CInt             -- ^ ldc
    -> IO ()

foreign import ccall unsafe "cblas_sgemm"
  cblas_sgemm_unsafe :: GemmFFI Float

foreign import ccall unsafe "cblas_dgemm"
  cblas_dgemm_unsafe :: GemmFFI Double

-- todo: can I use the pattern match on RealWorld# here ?
cblas_gemm_helper :: ( Num a, Storable a )
                  => GemmFFI a         -- ^ gemm function
                  -> MatrixOrder       -- ^ data ordering
                  -> ForeignPtr a      -- ^ matrix A
                  -> Transpose         -- ^ specifies whether to transpose matrix A
                  -> ForeignPtr a      -- ^ matrix B
                  -> Transpose         -- ^ specifies whether to transpose matrix B
                  -> Int               -- ^ Number of rows in matrices A and C
                  -> Int               -- ^ Number of columns in matrices B and C
                  -> Int               -- ^ Number of columns in matrix A; number of rows in matrix B
                  -> a                 -- ^ alpha
                  -> ForeignPtr a      -- ^ alpha * A * B
cblas_gemm_helper gemm_ffi RowMajor !aPtr !aT !bPtr !bT !m !n !k !alpha = unsafePerformIO $! do
  cPtr <- allocatePtr (m * n)

  let !transA = tranposeToCBLASTranpose aT
      !transB = tranposeToCBLASTranpose bT
      !order  = orderToCBLASEnum RowMajor
      !m'     = toCInt m
      !n'     = toCInt n
      !k'     = toCInt k

  withForeignPtr aPtr $ \aPtr' ->
    withForeignPtr bPtr $ \bPtr' ->
      withForeignPtr cPtr $ \cPtr' ->
        gemm_ffi order transA transB m' n' k' alpha aPtr' k' bPtr' n' 0 cPtr' n'

  return $! cPtr

cblas_sgemm :: MatrixOrder       -- ^ data ordering
            -> ForeignPtr Float  -- ^ matrix A
            -> Transpose         -- ^ specifies whether to transpose matrix A
            -> ForeignPtr Float  -- ^ matrix B
            -> Transpose         -- ^ specifies whether to transpose matrix B
            -> Int               -- ^ Number of rows in matrices A and C
            -> Int               -- ^ Number of columns in matrices B and C
            -> Int               -- ^ Number of columns in matrix A; number of rows in matrix B
            -> Float             -- ^ alpha
            -> ForeignPtr Float  -- ^ alpha * A * B
cblas_sgemm RowMajor !aPtr !aT !bPtr !bT !m !n !k !alpha =
  cblas_gemm_helper cblas_sgemm_unsafe RowMajor aPtr aT bPtr bT m n k alpha

cblas_dgemm :: MatrixOrder        -- ^ data ordering
            -> ForeignPtr Double  -- ^ matrix A
            -> Transpose          -- ^ specifies whether to transpose matrix A
            -> ForeignPtr Double  -- ^ matrix B
            -> Transpose          -- ^ specifies whether to transpose matrix B
            -> Int                -- ^ Number of rows in matrices A and C
            -> Int                -- ^ Number of columns in matrices B and C
            -> Int                -- ^ Number of columns in matrix A; number of rows in matrix B
            -> Double             -- ^ alpha
            -> ForeignPtr Double  -- ^ alpha * A * B
cblas_dgemm RowMajor !aPtr !aT !bPtr !bT !m !n !k !alpha =
  cblas_gemm_helper cblas_dgemm_unsafe RowMajor aPtr aT bPtr bT m n k alpha
