#include <stdio.h>
#include <tgmath.h>
#include <complex.h>
#include "testharness.h"

int main(void)
{
    float f1 = 1.0f;
    f1 = fabs(f1);
    float f = fabs(1.0f);
    double d = fabs(1.0);
    long double l = fabs(1.0l);

    float _Complex fc = 3.25f + 0.1if;
    // should directly have type float and not float _Complex
    float f2 = fabs(fc);

    double _Complex fcd = 3.25 + 0.1i;
    double f2d = fabs(fcd);

    // Those two calls should both have the same return type
    double _Complex idk = pow(fc, fcd);
    double _Complex idk2 = pow(fcd, fc);

    // Those should directly have type int not going through any casting
    int i = ilogb(d);
    int j = ilogb(f);

    long double idk3 = scalbn(l, 1);

    if(f != 1.0f)
        E(1);

    if(d != 1.0)
        E(2);

    if(l != 1.0l)
        E(3);

    return 0;
}
