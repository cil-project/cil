#include "testharness.h"
#include <stdnoreturn.h>


int main() {
    int x = _Alignof (int);
    SUCCESS;
}
