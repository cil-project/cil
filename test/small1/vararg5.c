#include "vararg5.h"

/* 
This file is a test for variable argument functions, where the call to 
print is made in a separate function, talking. This checks to see if we 
can retrieve the arguments in a variable argument function if we call a 
separate function.
*/

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>

void chatting(char* s, ...);

int main(int argc, char** argv) {
  int x, y;
  char* s;
  x = 5;
  y = 1;
  s = "hello";
  talking("This should be 5: %d\n", x);
  return 0;
}

void talking(char* s, ...) {
  va_list ap;
  va_start(ap, s);
  vfprintf(stdout, s, ap);
  va_end(ap);
}
