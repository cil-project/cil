//The Gnome calendar application uses initializers with arithmetic expressions:
#include "testharness.h"


int x = ! (3 && ! 3);

int y = ! &x;

int z = &x && (&y || ! 4);

int array[(3 && !3) ? 6 : 8];

int main() {
  return x - 1;
}

