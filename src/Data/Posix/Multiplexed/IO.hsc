{-#LANGUAGE ForeignFunctionInterface#-}
{-#LANGUAGE ScopedTypeVariables#-}

module Data.Posix.Multiplexed.IO where

import Data.Posix.Internal.FdSet
import Data.Posix.Internal.Timeval
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc

#include <sys/select.h>

foreign import ccall safe "select" c_select :: CInt -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO CInt

-- | A call to 'select' blocks untill the given file descriptors are
-- ready to perform I/O or untill an optionally specified timeout has
-- elapsed.
select :: CInt -> FdSet -> FdSet -> FdSet -> Maybe CTimeval -> IO CInt
select n (FdSet readfds) (FdSet writefds) (FdSet exceptfds) timeout = do
  withForeignPtr readfds $ \readPtr ->
      withForeignPtr writefds $ \writePtr ->
          withForeignPtr exceptfds $ \exceptPtr -> do
              timeoutPtr' <- timeoutPtr
              c_select n readPtr writePtr exceptPtr (castPtr timeoutPtr')
  where
    timeoutPtr = case timeout of
                   Nothing -> return nullPtr
                   Just tout -> do
                          (ptr :: Ptr CTimeval) <- malloc
                          poke ptr tout
                          return ptr
  
