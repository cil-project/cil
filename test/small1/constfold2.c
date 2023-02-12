#include "testharness.h"
int main()
{
    /* We check that with warnings as errors this fails also after CIL */
    int displayMask = 1 << 31;
    displayMask = 0;

    return displayMask;
}
