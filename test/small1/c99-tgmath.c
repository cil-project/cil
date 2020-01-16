#include <stdio.h>
#include <tgmath.h>
#include <complex.h>
#include "testharness.h"

int main(void)
{
    float f = fabs(1.0f);
    double d = fabs(1.0);
    long l = fabs(1.0l);

    float _Complex fc = 3.25f + 0.1if;
    float f2 = fabs(fc);

    double _Complex fcd = 3.25 + 0.1i;
    double f2d = fabs(fcd);

    if(f != 1.0f)
        E(1);

    if(d != 1.0)
        E(2);

    if(l != 1.0l)
        E(3);

    return 0;
}
