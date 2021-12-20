#include "testharness.h"

int pr(const char val)
{
    switch (val) {
    case '0' ... '7':
    case 'd':	/* KERN_DEFAULT */
        return 5;
    default:
        return 1;
    }

    return 2;
}

int main() {
    pr('a');
    SUCCESS;
}
