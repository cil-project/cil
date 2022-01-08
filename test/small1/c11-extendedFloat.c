#include "testharness.h"

_Float32 f32;
_Float64 f64;
_Float32x f32x;
_Float64x f64x;


int main() {
    if(sizeof(f32) != 4) {
        E(1);
    }

    if(sizeof(f64) != 8) {
        E(2);
    }

    SUCCESS;
}
