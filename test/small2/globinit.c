#include "../small1/testharness.h"
#include "../small1/testkinds.h"

// Test the global initializer

// NUMERRORS 2

struct S {
  struct {
    int *p1;
    int  i2;
    int * p3[4];
  } a[4];
  char *f2;
};

int i1 = 1, i2 = 2, i3 = 3, i4 = 4, i5 = 5;
int ia[8] = { 0, 1, 2, 3, 4, 5, 6, 7};

struct S g = { .a[0].p1 = &i1  , .a[1].p3[0] = ia, .f2 = "test" };


int main() {
  
#if 1 == ERROR
  //ERROR(1):Error 1
  {
    struct S * __WILD wg = &g; // Make g WILD
  
    // Test that the address is right
    if(HAS_KIND(&i1, WILD_KIND) &&
       g.a[0].p1 == &i1 && * g.a[0].p1 == 1) E(1); 
  }
#endif

  // Now make sure that we can write SEQ pointers
#if 2 == ERROR
  {
    int * __SEQ x = g.a[2].p3[1]; // Just to propagate the constraint
    // Make sure that we can read
    if(g.a[1].p3[0] [5] == 5) E(2); // ERROR(2):Error 2
  }
#endif  
  

  SUCCESS;
}
