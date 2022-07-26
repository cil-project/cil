#include <stdio.h>
#include <tgmath.h>
#include <complex.h>
#include "testharness.h"

typedef struct loc_t
{
  long nloc;
} loc_t;


void fun(const loc_t* loc) {
    long l =8;
    int n0 =(int)sqrt(l); // works
    int n1 =(int)sqrt(loc->nloc); // fails
}


int main() {
  loc_t loc;
  loc.nloc = 5;
  loc_t* ptr = &loc;
  fun(ptr);
  SUCCESS;
}
