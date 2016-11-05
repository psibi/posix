module Data.Posix.File.Constant where

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

-- $ Error constant and their descriptions

e2big :: Int
e2big = #const E2BIG

eacces :: Int
eacces = #const EACCES

eagain :: Int
eagain = #const EAGAIN

ebadf :: Int
ebadf = #const EBADF

ebusy :: Int
ebusy = #const EBUSY

echild :: Int
echild = #const ECHILD

edom :: Int
edom = #const EDOM

eexist :: Int
eexist = #const EEXIST

efault :: Int
efault = #const EFAULT

efbig :: Int
efbig = #const EFBIG

eintr :: Int
eintr = #const EINTR


-- $ Flags for open() system call 

o_append :: Int
o_append = #const O_APPEND

o_async :: Int
o_async = #const O_ASYNC

o_cloexec :: Int
o_cloexec = #const O_CLOEXEC

o_creat :: Int
o_creat = #const O_CREAT

o_rdonly :: Int
o_rdonly = #const O_RDONLY

o_trunc :: Int
o_trunc = #const O_TRUNC

o_wronly :: Int
o_wronly = #const O_WRONLY
