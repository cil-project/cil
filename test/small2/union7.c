#include "../small1/testharness.h"

// NUMERRORS 0

int my_get_tag(int thetag) {
  return thetag;
}


struct foo {
  int tag; // 0 for integer, 1 for pointer to int, 2 for structure 
  union __SELECTOR(my_get_tag("tag")) {
    int anint __SELECTEDWHEN(0);
    int * ptrintr __SELECTEDWHEN(1);
    struct {
      int * * ptrptr;
    } ptrptr __SELECTEDWHEN(2);
  } data;
} g;
  

int main() {

  if(KIND_OF(g.data.ptrintr) != SAFE_KIND) E(1);
  
  SUCCESS;
}
