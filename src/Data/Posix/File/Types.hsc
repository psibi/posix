{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Posix.File.Types where

#include <sys/time.h>

import Foreign.C
import Foreign.Storable

data CTimeval = CTimeval
  { tvSec :: CLong
  , tvUsec :: CLong
  } deriving (Show, Eq, Ord)

instance Storable CTimeval where
    sizeOf _ = (#size struct timeval)
    alignment _ = (#alignment struct timeval)
    peek ptr = do
      sec <- (#peek struct timeval, tv_sec) ptr
      usec <- (#peek struct timeval, tv_usec) ptr
      return $ CTimeval sec usec

    poke ptr (CTimeval sec usec) = do
      (#poke struct timeval, tv_sec) ptr sec 
      (#poke struct timeval, tv_usec) ptr usec
