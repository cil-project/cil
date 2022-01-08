#include "testharness.h"

_Float32 f32;
_Float64 f64;


int main() {
    if(sizeof(f32) != 4) {
        E(1);
    }

    if(sizeof(f64) != 8) {
        E(2);
    }

    SUCCESS;
}
