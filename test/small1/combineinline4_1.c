#include "testharness.h"

/* Test that we rename properly includes even if they have prototypes and
   if they are used before they are defined */
int foo(int x); /* Declare it here.  */

inline int foo(int x) { return x; } 

extern getfoo2();

int main() {
  if(getfoo2() != (int)foo) E(1);

  SUCCESS;
}
