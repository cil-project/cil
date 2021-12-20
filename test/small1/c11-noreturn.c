#include "testharness.h"
#include <stdnoreturn.h>

_Noreturn int fun() {
    SUCCESS;
}

noreturn int blub() {
    SUCCESS;
}

int blabla() __attribute__((noreturn));

int blabla() {
    SUCCESS;
}

int main() {
    fun();
    blub();
    blabla();

    SUCCESS;
}
