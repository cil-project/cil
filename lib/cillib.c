// cillib.c
// stuff that gets linked with cil'd programs *and* box'd programs

#include <ctype.h>      // __ctype_b
#include <stdio.h>      // FILE, fprintf
#include <stdlib.h>     // abort

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

#if defined(__GLIBC__) && __GLIBC__ == 2
/// For brooksie
// sm: glibc 2.0 and 2.1 both have this; so I removed check for minor number
int _get__ctype_b(int chr) { 
  if (!( (unsigned)chr < 256 )) {
    fprintf(stderr, "Bad character index to _get__ctype_b: %d\n", chr);
    abort();
  }
  return (__ctype_b[chr]);
}
#endif
 
