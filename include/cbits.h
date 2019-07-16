#include <sys/select.h>

void fd_clr_wrapper(int fd, fd_set* set);
int fd_isset_wrapper(int fd, fd_set* set);
void fd_set_wrapper(int fd, fd_set* set);
void fd_zero_wrapper(fd_set* set);

struct test {
  struct test *pNext;
  char * testField1;
  char * testField2;
  char testField3[3];
};

struct test jam();

struct test *jam2();
