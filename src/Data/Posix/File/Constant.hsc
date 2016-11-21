module Data.Posix.File.Constant
  ( module Data.Posix.File.Constant
  , module Foreign.C.Error
  ) where

import Data.Int (Int32)
import Foreign.C.Error

#define _GNU_SOURCE
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>

eOF :: Int32
eOF = #const EOF

-- $ Flags for open() system call 

oAPPEND :: Int32
oAPPEND = #const O_APPEND

oASYNC :: Int32
oASYNC = #const O_ASYNC

oCLOEXEC :: Int32
oCLOEXEC = #const O_CLOEXEC

oCREAT :: Int32
oCREAT = #const O_CREAT

oRDONLY :: Int32
oRDONLY = #const O_RDONLY

oTRUNC :: Int32
oTRUNC = #const O_TRUNC

oWRONLY :: Int32
oWRONLY = #const O_WRONLY

-- $ Flags for synchronized I/O

oSYNC :: Int32
oSYNC = #const O_SYNC

-- | In Linux, both 'oDSYNC' and 'ORSYNC' are synonymous with
-- 'OSYNC'. They are implemented just for completion.

oDSYNC :: Int32
oDSYNC = #const O_DSYNC

oRSYNC :: Int32
oRSYNC = #const O_RSYNC

-- $ Flag for Direct I/O

oDIRECT :: Int32
oDIRECT = #const O_DIRECT

-- $ Positional I/O flags

sEEKCUR :: Int32
sEEKCUR = #const SEEK_CUR

sEEKEND :: Int32
sEEKEND = #const SEEK_END

sEEKSET :: Int32
sEEKSET = #const SEEK_SET
