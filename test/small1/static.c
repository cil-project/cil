#include <stdio.h>

int counter() {
    static int counter = 0;
    counter = counter + 1;
    return counter;
}

int main() {
    if (counter() != 1) {
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
