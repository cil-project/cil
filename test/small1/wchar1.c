#include "testharness.h"
#include <stddef.h>

int main() {
  wchar_t *wbase = L"Hello" L", world";
  char * w = (char *)wbase;
  char * s =  "Hello" ", world";
  int i;

  for (i=0; i < 10; i++) {
    if (w[i * sizeof(wchar_t)] != s[i]) {
      E(1); 
    } 
    if (w[i * sizeof(wchar_t)+ 1] != 0) {
      E(2);
    } 
  }
  SUCCESS;
}
