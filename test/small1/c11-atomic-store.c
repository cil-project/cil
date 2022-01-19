#include "testharness.h"
#include <stdnoreturn.h>
#include <stdatomic.h>

typedef _Atomic _Bool atomic_bool;


int main() {
    atomic_int ato;
    _Atomic int* ptr = &ato;

    __extension__ ({
        __auto_type blub = ato;
        ato = 8;
    });


    // Extended version from GCC
    __extension__ ({ __auto_type __atomic_store_ptr = (

    ptr
    ); __typeof__ ((void)0, *__atomic_store_ptr) __atomic_store_tmp = (

    17

    ); __atomic_store (__atomic_store_ptr, &__atomic_store_tmp, (5)); })
                        ;


    atomic_store(ptr,17);
    atomic_store_explicit(ptr,17,memory_order_relaxed);
    SUCCESS;
}
