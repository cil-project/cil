#include "testharness.h"

struct A {
  struct S { int x; } s;
  int a[4];
  int *p;
};

struct S s;

struct B {
  struct A a1;
  struct A a2;
  struct A a3;
  struct A a4;
  struct A a5;
};

struct A a;


int main() {
  struct B b = { .a2 = { .s = { .x = 5 }},
                 s, 0, 0, 0, 0, 0,  // a3
                 6, { 0 }, 0,  // a4
                 a // a5
  } ;
  
  if(b.a1.a != 5) E(1);
}

