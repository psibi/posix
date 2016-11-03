{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables#-}

module Data.C.File.IO where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (free)
import System.Posix.Types (CSsize(..))
import Data.ByteString

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

foreign import ccall unsafe "open" c_open ::
               CString -> CInt -> IO CInt

open :: String -> Int -> IO Int
open pathname flags = do
  str' <- newCString pathname
  ret <- c_open str' (fromIntegral flags)
  free str'
  return $ fromIntegral ret

foreign import ccall unsafe "close" c_close :: CInt -> IO ()

close :: Int -> IO ()
close = c_close . fromIntegral

foreign import ccall unsafe "read" c_read :: CInt -> Ptr () -> CSize -> IO CSsize

read :: Int -- ^ file descriptor
     -> Int64 -- ^ Bytes to allocate
     -> Word64 -- ^ Read upto this many bytes
     -> IO (ByteString, Int64)
read fd bytes len = do
  (ptr :: Ptr ()) <- mallocBytes (fromIntegral bytes)
  size <- c_read (fromIntegral fd) ptr (fromIntegral len)
  bstring <- packCString (castPtr ptr)
  free ptr
  return (bstring, (fromIntegral size))
