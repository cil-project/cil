#include "testharness.h"

/* This program runs correctly under GCC */

int foo(int x) {
  return x; 
}

int bar(int *x) {
  return *x; 
} 

int main() {
  int (*fptr)(int);     // this declaration causes CCured to fail
  // int (*fptr)();             // this one is OK
  // int (*fptr)(int *);        // this one is OK
  int result = 0;

  fptr = foo;
  result = (*fptr)(3);

  fptr = bar; 
  result += (*fptr)(&result); 

  if (result != 6) {
    E(1);
  } 
  // printf("Hey, we got %d!\n", result);
  SUCCESS;
} 
