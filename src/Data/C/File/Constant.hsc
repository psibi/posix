module Data.C.File.Constant where

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

o_append :: Int
o_append = #const O_APPEND

o_async :: Int
o_async = #const O_ASYNC

o_cloexec :: Int
o_cloexec = #const O_CLOEXEC

o_creat :: Int
o_creat = #const O_CREAT

main = print $ [o_append, o_async, o_cloexec, o_creat]
