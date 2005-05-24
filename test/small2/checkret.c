/* Tests for checking the return values */
#include "../small1/testharness.h"
// NUMERRORS 6


struct str1 {
  int  x1;
  int *x2;
  struct {
    int i1;
    int *i2;
  } x3;
};

int global;

#if ERROR <= 2
struct str1 retstr() {
  int local;
  struct str1 res = { 0, &global, 1, &global }; // ERROR(0)
  struct str1 res = { 0, &local, 1, &global }; // ERROR(1):Returning a local
  struct str1 res = { 0, &global, 1, &local }; // ERROR(2):Returning a local
  return res;
}

#elif ERROR <= 4
struct strarr {
  int i1;
  int *a[7];
};

struct strarr retarr() {
  int local;
  struct strarr res = { 0, &global, &global, &global }; // ERROR(3):Error 3
  struct strarr res = { 0, &global, &local, &global }; // ERROR(4):Returning a local
  return res;
}

#elif ERROR <= 5
union unfoo {
  struct { int *e1; int *e2; int *e3; int *e4; } f1;
  int *f2[4];
};

union unfoo retunion() {
  int local;
  union unfoo res = { &global, &local, &global }; // ERROR(5):Returning a local
  return res;
}
#else
union unempty { } retunempty() {
  union unempty res;
  return res;
}
#endif

int main() {
#if ERROR <= 2  
  retstr();
#elif ERROR <= 4
  retarr();
#elif ERROR <= 5  
  retunion();
#else
  retunempty(); E(6);//ERROR(6):Error 6
#endif  

  E(3); // ERROR(3)
  SUCCESS;
}
