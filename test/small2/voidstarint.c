#include "../small1/testharness.h"


int main() {
    int i1 = 5;
    void **vi5 = &i1;
    void *vi6 = vi5;
    int * * pi2 = vi5;
    return * * pi2;
}
