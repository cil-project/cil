#include "testharness.h"

typedef int INT;

struct {
  INT i;
  int x;
} g;


int main() {
  E(1); // Should not compile
}
