#include "../small1/testharness.h"

// NUMERRORS 0

struct foo {
  int a[8];
  int *b;
} gfoo;

struct bar {
  int a[8];
  int *b;
} gbar;

int main() {
  int * __INDEX p = & gfoo.a[2]; // Force gfoo.a to have a length

  // This should be Ok, but pbar->b is gfoo.a[7]
  struct bar *pbar = (struct bar*)&gfoo;

  gfoo.a[7] = 5;

  printf("Pointer is %lx\n", (unsigned long)pbar->b);
  *(pbar->b) = 0; // Boom
  SUCCESS;
}
