#include "testharness.h"

unsigned long long x1 = 0xff00000000000000ULL;

int main() {
  if(x1 >> 56 != 255) E(1);
  SUCCESS;
}
