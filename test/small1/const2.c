#include "testharness.h"

int main() {

  if (((unsigned)0 - 1 >= 0) == 0)
    E(1);

  SUCCESS;

  return 0;
}
