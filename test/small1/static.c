#include <stdio.h>
extern void exit(int);

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
    if (counter() != 19) {
        printf("Error in first count.\n");
        exit(1);
    }
    if (counter() != 20) {
        printf("Error in second count (static variable failure?).  Aborting!\n");
        exit(1);
    }
    printf("Passed.\n");
    return 0;
}
