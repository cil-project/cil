#include "testharness.h"

int main() {
  char * w = L"Hello" L", world";
  char * s =  "Hello" ", world";
  int i;

  for (i=0; i < 10; i++) {
    if (w[i * 2] != s[i]) {
      E(1); 
    } 
    if (w[i * 2 + 1] != 0) {
      E(2);
    } 
  }
  SUCCESS;
}
