#include<stdbool.h>
#include<stdio.h>
#include "testharness.h"
int main(void)
{
    bool b = 17;
    _Bool b1 = 17;

    if(b == 1) {

    } else {
        E(1);
    }

    if(b1 == 1) {

    } else {
        E(2);
    }
}
