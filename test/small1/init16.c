//The Gnome calendar application uses initializers with arithmetic expressions:
#include "testharness.h"


int x = ! (3 && ! 3);

int y = ! &x;

int z = &x && &y;


int main() {
  return x - 1;
}

