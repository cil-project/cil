#include <ctype.h>
#include <stdio.h>

FILE * get_stdin() {
    return stdin;
}
FILE * get_stdout() {
    return stdout;
}
FILE * get_stderr() {
    return stderr;
} 

#ifdef __CYGWIN__
char *get_ctype() {
  return _ctype_;
}
#endif

