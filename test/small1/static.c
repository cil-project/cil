#include <stdio.h>

int counter() {
    static int counter = 18;
    counter = counter + 1;
    return counter;
}


int s1;

int sets1() {
  static int s1 = 5; // Our own private copy

  static int counter = 29; // Try again
  return s1 + counter;
}

int main() {
    if (counter() != s1) {
        printf("Error in first count.\n");
        abort();
    }
    if (counter() != 2) {
        printf("Error in second count (static variable failure?).  Aborting!\n");
        abort();
    }
    printf("Passed.\n");
    return 0;
}
