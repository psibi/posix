{-# LANGUAGE ForeignFunctionInterface #-}

module Data.C.Error where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc (free)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "stdio.h perror" c_perror ::
               CString -> IO ()

perror :: String -> IO ()
perror str = do
  str' <- newCString str
  c_perror str'
  free str'

foreign import ccall unsafe "string.h strerror" c_strerror ::
               CInt -> IO CString

strerror :: Int -> String
strerror x =
  unsafePerformIO $
  do cstr <- c_strerror (fromIntegral x)
     str <- peekCString cstr
     return str

foreign import ccall unsafe "string.h strerror_r" c_strerror_r ::
               CInt -> CString -> CSize -> IO CString

strerror_r :: Int -> Word64 -> String
strerror_r errnum size =
  unsafePerformIO $
  do cstr <-
       allocaBytes
         (fromIntegral size)
         (\ptr -> c_strerror_r (fromIntegral errnum) ptr (fromIntegral size))
     str <- peekCString cstr
     return str
