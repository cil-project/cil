#include "../small1/testharness.h"

#define NUMERRORS 1

/* A test case with tagged unions */
#ifndef __TAGGED
 #define __TAGGED
#endif

union u {
  int  f1;
  int* f2;
  struct {
    int *f3_1;
    int *f3_2;
  }    f3;
  int* (*f4)(int*);
} __TAGGED;


int main() {
  int i, i1;
  union u x;
  union u *px = &x;
  x.f1 = 5;
  if(x.f1 != 5) E(1);

  x.f2 = &i;
  if(px->f2 != &i) E(2);

  x.f3.f3_1 = &i1;
  x.f3.f3_2 = &i;
  if(x.f3.f3_1 != &i1) E(4);

  // And some trick with the thing appearing both on left and right-side
  x.f2 = x.f3.f3_1;
  if(px->f2 != &i) E(5);

  if(! HAS_KIND(px, SAFE_KIND)) E(10);
  
  SUCCESS;
}
