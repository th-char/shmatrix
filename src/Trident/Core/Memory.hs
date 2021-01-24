{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedSums           #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Trident.Core.Memory where

import           Data.Kind
import           Data.Proxy
import           Foreign.ForeignPtr
import           Foreign.Storable
import           GHC.ForeignPtr
import           GHC.TypeLits
import           System.IO.Unsafe

allocatePtr :: forall a. Storable a => Int -> IO (ForeignPtr a)
allocatePtr n =
  let dummy = undefined :: a
      bytes = n * sizeOf dummy
  in mallocPlainForeignPtrBytes bytes
{-# INLINE allocatePtr #-}
