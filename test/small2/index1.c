#include "../small1/testharness.h"

// NUMERRORS 1

struct foo {
  int a[8];
  int *b;
} gfoo;

struct bar {
  int a[8];
  int *b;
};

int main() {
  int * __INDEX p = & gfoo.a[2]; // Force gfoo.a to have a length

  // This should be Ok, but pbar->b is gfoo.a[7]
  struct bar *pbar = (struct bar*)&gfoo;

  gfoo.a[7] = 5;
  pbar->b = 0; 

  printf("Pointer is %lx\n", (unsigned long)pbar->b);
  *(pbar->b) = 0; //ERROR(1): Null
  SUCCESS;
}
