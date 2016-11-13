module Data.Posix.File.Constant
( module Foreign.C.Error ) where

import Data.Int (Int32)
import Foreign.C.Error

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

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

sEEK_CUR :: Int32
sEEK_CUR = #const SEEK_CUR

sEEK_END :: Int32
sEEK_END = #const SEEK_END

sEEK_SET :: Int32
SEEK_SET = #const SEEK_SET
