#include "../small1/testharness.h"

// NUMERRORS 3

union {  //size = 12
  struct {
    int *a, *b;
  } f1;
  int f2;

  // An ugly, unrealistic case
  struct {  //size = 12
    union {  //size = 8
      int x;
      struct {  //size = 8
        int* s1;
        int* s2;
      } s;
    } __TAGGED f3_u;
    int f3_int;
  } f3;

} __TAGGED u;

int i;

int main() {
  
  u.f2 = 5; // now u.f1.a = 5
  u.f1.b = &i; // now the tag says that u.f1 is active

  i = * u.f1.a; //ERROR(1): Null pointer

  u.f2 = 5; // now u.f3.f3_u.s.s1 = 5

  //Union in a union.  This will clear the f3 struct and (redundantly) the 
  // f3.f3_u.s struct.
  u.f3.f3_u.s.s2 = &i;

  i = * u.f3.f3_u.s.s1; //ERROR(2): Null pointer
  i = * u.f1.b; //ERROR(3): WRONGFIELD

  SUCCESS;
}
