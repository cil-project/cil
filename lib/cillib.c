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
int _get_ctype_p1(unsigned int chr) {
  return (_ctype_ + 1)[chr]; 
}
#endif

#if defined(__GLIBC__) && __GLIBC__ == 2 && __GLIBC_MINOR__ == 0
/// For brooksie
int _get__ctype_b(int chr) { 
  return (__ctype_b[chr]);
}
#endif
 
