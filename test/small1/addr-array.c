#include "testharness.h"

int main() {

  int a[10];

  // (&a) is a pointer to an array of 10 integers,
  // a is a pointer to integer
  
  if ((void*)((&a)+1) == (void *)(a+10))
    SUCCESS
  else
    E(1)
}
