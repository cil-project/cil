#include "testharness.h"

typedef struct foo *PFOO;

typedef struct foo {
  double x;
  PFOO y;
} *PTR;

PTR *g3;

int main() {
  main1();
  main2();
  // Make sure that the offset is right
  if(& g3->y != & ((struct { double x; PFOO y} *)g3)->y) E(1);

  SUCCESS;
}
