#include "testharness.h"

int main() {
  char * w = L'W';      // wide character constant
  char * s =  "W"; 
  int i;

  for (i=0; i < 1; i++) {
    if (w[i * 2] != s[i]) {
      E(1); 
    } 
    if (w[i * 2 + 1] != 0) {
      E(2);
    } 
  }
  SUCCESS;
}
