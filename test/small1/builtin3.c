#include "testharness.h"

int main (void)
{
    int x = 0;
    int b = __builtin_constant_p (x++);

    int arr[];

    __builtin_constant_p (x++);
    
    b = __builtin_constant_p (x++);

    if(! __builtin_constant_p(56 + 34)) { x ++; }
    
    // The x++ should happen only once
    (__extension__ (__builtin_constant_p (x++) ? 0 : x++));

    switch(8) {
      case (__builtin_constant_p (x++) ? x : 8):
        break;
    default:
      E(2);
    }
    
    if(x != 1) E(1);
    SUCCESS;
 }
