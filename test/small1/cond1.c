#include "testharness.h"

int main() {

  int x1 = ({goto L1; 0;}) && ({L1: 5;});

  printf("x1 = %d\n", x1);

  if(x1 != 1) E(1);

  SUCCESS;
}
