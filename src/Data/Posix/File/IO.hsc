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
  -- * Positional I/O
  , lseek
  , pread
  , pwrite
  -- * Truncate Files
  , ftruncate
  , truncate
  -- * Other functions
  ) where

import Prelude hiding (read, truncate)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.Posix.Types (CSsize(..), CMode(..))
import Data.Int (Int32)
import Data.ByteString

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

foreign import ccall unsafe "open" c_open ::
               CString -> CInt -> IO CInt

open :: String -> Int32 -> IO Int32
open pathname flags = do
  str' <- newCString pathname
  ret <- c_open str' (fromIntegral flags)
  free str'
  return $ fromIntegral ret

foreign import ccall unsafe "open" c_open_mode ::
               CString -> CInt -> CMode -> IO CInt

openMode :: String -> Int32 -> CMode -> IO Int32
openMode pathname flags mode = do
  str' <- newCString pathname
  ret <- c_open_mode str' (fromIntegral flags) mode
  free str'
  return $ fromIntegral ret

foreign import ccall safe "read" c_read ::
               CInt -> Ptr () -> CSize -> IO CSsize

read
  :: Int32 -- ^ file descriptor
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

close :: Int32 -> IO ()
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
  :: Int32 -- ^ File descriptor
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
  :: Int32 -- ^ File descriptor
  -> IO Int32
fsync fd = do
  ret <- c_fsync $ fromIntegral fd
  return $ fromIntegral ret

foreign import ccall safe "fdatasync" c_fdatasync ::
               CInt -> IO CInt

fdatasync
  :: Int32 -- ^ File descriptor
  -> IO Int32
fdatasync fd = do
  ret <- c_fdatasync $ fromIntegral fd
  return $ fromIntegral ret

foreign import ccall safe "sync" sync :: IO ()

-- Regarding off_t:
-- http://stackoverflow.com/questions/9073667/where-to-find-the-complete-definition-of-off-t-type
foreign import ccall unsafe "lseek" c_lseek ::
               CInt -> (#type off_t) -> CInt -> IO (#type off_t)

lseek :: Int32 -- ^ File descriptor
      -> (#type off_t) -- ^ File position
      -> Int32 -- ^ Origin Flag
      -> IO (#type off_t)
lseek fd pos origin = c_lseek (fromIntegral fd) pos (fromIntegral origin)

foreign import ccall safe "pread" c_pread :: CInt -> Ptr () -> CSize -> (#type off_t) -> IO (#type ssize_t)

pread :: Int32 -- ^ File descriptor
      -> Int64 -- ^ Bytes to read
      -> (#type off_t) -- ^ File position
      -> IO (ByteString, #type ssize_t)
pread fd bytes pos = do
  (ptr :: Ptr ()) <- mallocBytes (fromIntegral bytes)
  size <- c_pread (fromIntegral fd) ptr (fromIntegral bytes) pos
  bstring <- packCStringLen (castPtr ptr, fromIntegral size)
  free ptr
  return (bstring, size)

foreign import ccall safe "pwrite" c_pwrite :: CInt -> Ptr () -> CSize -> (#type off_t) -> IO (#type ssize_t)

pwrite :: Int32 -- ^ File descriptor
       -> ByteString
       -> (#type off_t) -- ^ File position
       ->  IO (#type ssize_t)
pwrite fd bs pos =
  useAsCStringLen
    bs
    (\(ptr, len) -> do
       size <- c_pwrite (fromIntegral fd) (castPtr ptr) (fromIntegral len) pos
       return $ fromIntegral size)

foreign import ccall safe "ftruncate" c_ftruncate :: CInt -> (#type off_t) -> IO CInt

ftruncate :: Int32 -- ^ File descriptor
          -> (#type off_t) -- ^ Truncates given file to this length
          -> IO Int32
ftruncate fd len = do
  res <- c_ftruncate (fromIntegral fd) len
  return $ fromIntegral res

foreign import ccall safe "truncate" c_truncate :: CString -> (#type off_t) -> IO CInt

truncate :: ByteString -- ^ File name
         -> (#type off_t) -- ^ Truncates given file to this length
         -> IO Int32
truncate bs len = useAsCString bs (\str -> do
                                     size <- c_truncate str len
                                     return $ fromIntegral size)
