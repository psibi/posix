{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Posix.Internal.FdSet where

import Foreign.ForeignPtr
import Foreign.Ptr
import System.Posix.Types
import Foreign.C.Types (CInt(..))

-- fd_set is an opaque type

#include <sys/select.h>
#include "cbits.h"

data FdSet =
  FdSet (ForeignPtr ())

fdClr :: Fd -> FdSet -> IO ()
fdClr fd (FdSet ptr) = withForeignPtr ptr (c_fd_clr_wrapper (fromIntegral fd))

-- | Returs non zero integer if the file descriptor is in the set.
fdIsSet :: Fd -> FdSet -> IO CInt
fdIsSet fd (FdSet ptr) =
  withForeignPtr ptr (c_fd_isset_wrapper (fromIntegral fd))

fdSet :: Fd -> FdSet -> IO ()
fdSet fd (FdSet ptr) = withForeignPtr ptr (c_fd_set_wrapper (fromIntegral fd))

fdZero :: FdSet -> IO ()
fdZero (FdSet ptr) = withForeignPtr ptr (c_fd_zero_wrapper)

emptyFdSet :: IO FdSet
emptyFdSet = do
  ptr <- mallocForeignPtrBytes #{size fd_set}
  return $ FdSet ptr

foreign import ccall "cbits.h fd_zero_wrapper" c_fd_zero_wrapper ::
               Ptr () -> IO ()

foreign import ccall "cbits.h fd_set_wrapper" c_fd_set_wrapper ::
               CInt -> Ptr () -> IO ()

foreign import ccall "cbits.h fd_clr_wrapper" c_fd_clr_wrapper ::
               CInt -> Ptr () -> IO ()

foreign import ccall "cbits.h fd_isset_wrapper" c_fd_isset_wrapper
               :: CInt -> Ptr () -> IO CInt
