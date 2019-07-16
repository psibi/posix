#include <sys/select.h>
#include "cbits.h"
#include <stdio.h>

void fd_clr_wrapper(int fd, fd_set* set)
{
  FD_CLR(fd, set);
}

int fd_isset_wrapper(int fd, fd_set* set)
{
  return FD_ISSET(fd, set);
}

void fd_set_wrapper(int fd, fd_set* set)
{
  FD_SET(fd, set);
}

void fd_zero_wrapper(fd_set* set)
{
  FD_ZERO(set);
}

struct test a = {
                 .pNext = NULL,
                 .testField1 = "helloko",
                 .testField2 = "byeko",
                 .testField3 = "aaa"
};

struct test jam() {
  struct test a = {
    .pNext = NULL,               
    .testField1 = "hello",
    .testField2 = "bye",
    .testField3 = "ABC"
  };
  return a;
}

struct test *jam2() {
  return &a;
}


  
