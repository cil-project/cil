#include "testharness.h"

int f1() { return 1;}

int foo() {
  static int bar(); // This refers to the outside function "bar"
  static int (*pbar)() = f1;
  
  return bar() + pbar();
}

static int bar() {
  return 55;
}

static int (*pbar)() = bar;

int main() {
  if(foo() != 56) E(1); // Foo invokes bar + f1

  if(pbar() != 55) E(2); // We have two copies of pbar

  SUCCESS;
}
