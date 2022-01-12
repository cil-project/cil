#include "testharness.h"
#include <stdnoreturn.h>
#include <stdatomic.h>

typedef _Atomic _Bool atomic_bool;


int main() {
    atomic_int atomic;
    int* ptr = &atomic;

    atomic_store(ptr,17);
    atomic_store_explicit(ptr,17,memory_order_consume);
    SUCCESS;
}
