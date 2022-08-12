// Technically not C11 but ISO/IEC TS 18661-3:2015
#include "testharness.h"

_Float32 f32;
_Float64 f64;
_Float32x f32x;
#if __HAVE_FLOAT64X
_Float64x f64x;
#endif


int main() {
    if(sizeof(f32) != 4) {
        E(1);
    }

#if __HAVE_FLOAT64X
    if(sizeof(f64) != 8) {
        E(2);
    }
#endif

    SUCCESS;
}
