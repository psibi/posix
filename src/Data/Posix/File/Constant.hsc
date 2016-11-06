module Data.Posix.File.Constant where

import Data.Int (Int32)

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

-- $ Error constant and their descriptions

e2big :: Int32
e2big = #const E2BIG

eacces :: Int32
eacces = #const EACCES

eagain :: Int32
eagain = #const EAGAIN

ebadf :: Int32
ebadf = #const EBADF

ebusy :: Int32
ebusy = #const EBUSY

echild :: Int32
echild = #const ECHILD

edom :: Int32
edom = #const EDOM

eexist :: Int32
eexist = #const EEXIST

efault :: Int32
efault = #const EFAULT

efbig :: Int32
efbig = #const EFBIG

eintr :: Int32
eintr = #const EINTR


-- $ Flags for open() system call 

o_append :: Int32
o_append = #const O_APPEND

o_async :: Int32
o_async = #const O_ASYNC

o_cloexec :: Int32
o_cloexec = #const O_CLOEXEC

o_creat :: Int32
o_creat = #const O_CREAT

o_rdonly :: Int32
o_rdonly = #const O_RDONLY

o_trunc :: Int32
o_trunc = #const O_TRUNC

o_wronly :: Int32
o_wronly = #const O_WRONLY

-- $ Flags for synchronized I/O

oSYNC :: Int32
oSYNC = #const O_SYNC

-- | In Linux, both 'oDSYNC' and 'ORSYNC' are synonymous with
-- 'OSYNC'. They are implemented just for completion.

oDSYNC :: Int32
oDSYNC = #const O_DSYNC

oRSYNC :: Int32
oRSYNC = #const O_RSYNC
