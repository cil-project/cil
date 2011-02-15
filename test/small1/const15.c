#include "testharness.h"

unsigned long x = 0xfffffffffffffffeUL;

int main(){
    /* CIL used to truncate unsigned long constants to 32-bits, even on
     * 64-bits machines */
    if(sizeof(unsigned long) == 8 &&
            x == 0xfffffffeUL)
        E(1);

    SUCCESS;
    return 0;
}
