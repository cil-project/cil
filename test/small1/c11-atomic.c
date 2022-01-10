#include "testharness.h"
#include <stdnoreturn.h>

_Atomic const int * p1;  // p is a pointer to an atomic const int
const _Atomic(int) * p3; // same
_Atomic(int) const * p3;
// _Atomic(int*) const  p4; // unsupported as of now

typedef _Atomic _Bool atomic_bool;


int main() {
    SUCCESS;
}
