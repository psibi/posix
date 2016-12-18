{-#LANGUAGE ForeignFunctionInterface#-}

module Data.Posix.Internal.Timeval where

import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr

#include <sys/time.h>

data CTimeval = CTimeval CLong CLong

instance Storable CTimeval where
    sizeOf _ = (sizeOf (undefined :: CLong)) * 2
    alignment _ = alignment (undefined :: CLong)
    peek p = do
        s   <- peekElemOff (castPtr p) 0
        mus <- peekElemOff (castPtr p) 1
        return (CTimeval s mus)
    poke p (CTimeval s mus) = do
        pokeElemOff (castPtr p) 0 s
        pokeElemOff (castPtr p) 1 mus
