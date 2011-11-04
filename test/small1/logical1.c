#include "testharness.h"

int main() {
  long long x = 0x100000000LL;
  int i = 1;
  if (i && x) SUCCESS;
  E(1);
}
