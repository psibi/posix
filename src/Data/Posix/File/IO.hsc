{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables#-}

module Data.Posix.File.IO
  ( open
  , openMode
  , close
  , read
  , creat
  , write
  -- * Synchronized I/O
  , fsync
  , fdatasync
  , sync                                           
  ) where

import Prelude hiding (read)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.Posix.Types (CSsize(..), CMode(..))
import Data.ByteString

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

foreign import ccall unsafe "open" c_open ::
               CString -> CInt -> IO CInt

open :: String -> Int -> IO Int
open pathname flags = do
  str' <- newCString pathname
  ret <- c_open str' (fromIntegral flags)
  free str'
  return $ fromIntegral ret

foreign import ccall unsafe "open" c_open_mode ::
               CString -> CInt -> CMode -> IO CInt

openMode :: String -> Int -> CMode -> IO Int
openMode pathname flags mode = do
  str' <- newCString pathname
  ret <- c_open_mode str' (fromIntegral flags) mode
  free str'
  return $ fromIntegral ret

foreign import ccall safe "read" c_read ::
               CInt -> Ptr () -> CSize -> IO CSsize

read
  :: Int -- ^ file descriptor
  -> Int64 -- ^ Bytes to allocate
  -> Word64 -- ^ Read upto this many bytes
  -> IO (ByteString, Int64)
read fd bytes len = do
  (ptr :: Ptr ()) <- mallocBytes (fromIntegral bytes)
  size <- c_read (fromIntegral fd) ptr (fromIntegral len)
  bstring <- packCStringLen (castPtr ptr, fromIntegral size)
  free ptr
  return (bstring, (fromIntegral size))

foreign import ccall unsafe "close" c_close :: CInt -> IO ()

close :: Int -> IO ()
close = c_close . fromIntegral

-- | create() system call
foreign import ccall unsafe "creat" c_creat ::
               CString -> CMode -> IO ()

creat :: String -> CMode -> IO ()
creat name mode = do
  str <- newCString name
  c_creat str mode
  free str

foreign import ccall safe "write" c_write ::
               CInt -> Ptr () -> CSize -> IO CSsize

-- | write() system call

write
  :: Int -- ^ File descriptor
  -> ByteString
  -> IO Int64
write fd bs =
  useAsCStringLen
    bs
    (\(ptr, len) -> do
       size <- c_write (fromIntegral fd) (castPtr ptr) (fromIntegral len)
       return $ fromIntegral size)

foreign import ccall safe "fsync" c_fsync :: CInt -> IO CInt

fsync
  :: Int -- ^ File descriptor
  -> IO Int
fsync fd = do
  ret <- c_fsync $ fromIntegral fd
  return $ fromIntegral ret

foreign import ccall safe "fdatasync" c_fdatasync ::
               CInt -> IO CInt

fdatasync
  :: Int -- ^ File descriptor
  -> IO Int
fdatasync fd = do
  ret <- c_fdatasync $ fromIntegral fd
  return $ fromIntegral ret

foreign import ccall safe "sync" sync :: IO ()
