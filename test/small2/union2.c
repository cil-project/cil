#include "../small1/testharness.h"
#include "../small1/testkinds.h"

// NUMERRORS 1

union { 
    int *f1;
    int *f2[2];
    struct { int *a1, a2, *a3; } f3;
} * x;

int main() {
  if(HAS_KIND(x, "WILD")) E(1); //ERROR(1):Error 1
  SUCCESS;
}
