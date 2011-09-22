#include "testharness.h"

int global;

void init()
{
    global = 1;
}


int main()
{
    global = 0;
    init();

    if (global) {
        global = 2;
    } else {
        E(1);
    }
    return 0;
}
