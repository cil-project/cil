#include "testharness.h"
#include <stdnoreturn.h>

_Noreturn int fun() {
    return 5;
}

noreturn int blub() {
    return 7;
}

int blabla() __attribute__((noreturn));

int blabla() {
    return 8;
}

int main() {
    fun();
    blub();
    blabla();

    SUCCESS;
}
