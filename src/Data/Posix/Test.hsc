{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Posix.Test where

import Foreign.C
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Data.Word
import Data.Int

#include "cbits.h"

data TestS = TestS { next :: Ptr TestS, field1 :: CString, field2 :: CString, field3 :: Ptr Int8 }

instance Storable TestS where
    sizeOf _ = (#size struct test)
    alignment _ = (#alignment struct test)
    peek ptr = do
      pnext <- (#peek struct test, pNext) ptr
      field1 <- (#peek struct test, testField1) ptr
      field2 <- (#peek struct test, testField2) ptr
      field3 <- (#peek struct test, testField3) ptr
      return $ TestS pnext field1 field2 field3

    poke ptr (TestS pnext sec usec field3) = do
      (#poke struct test, pNext) ptr pnext
      (#poke struct test, testField1) ptr sec 
      (#poke struct test, testField2) ptr usec
      (#poke struct test, testField3) ptr field3

foreign import ccall "cbits.h jam2" c_jam2 :: IO (Ptr TestS)


