#include "../small1/testharness.h"

// NUMERRORS 1

union {
  struct {
    int *a, *b;
  } f1;
  int f2;
} __TAGGED u;

int i;

int main() {
  
  u.f2 = 5; // now u.f1.a = 5
  u.f1.b = &i; // now the tag says that u.f1 is active

  i = * u.f1.a; //ERROR(1): Null-pointer
}
