#include "../small1/testharness.h"
#include "../small1/testkinds.h"

// NUMERRORS 13


int *gptr;

int global;

int main() {
  int local;

  // This should work
  gptr = &global; //ERROR(0)

  // This should fail
  gptr = &local; // ERROR(1):Stack address

  // Play a trick with pointer arithemtic (SEQ)
#if ERROR == 2
  {
    // ERROR(2):Stack address
    int *t = &local; t += (&global - t);
    if(! HAS_KIND(t, "SEQ")) E(2);
    gptr = t; // Should fail
    local = *(gptr + (&local - gptr));
  }
#endif

  // The same trick with WILD
#if ERROR == 3
  {
    // ERROR(3):Stack address
    int *t = (int**)&local; // t is WILD now
    if(! HAS_KIND(t, "WILD")) E(3);
    t += (&global - t); gptr = t; // Should fail
  }
#endif
  
  // The same trick with FSEQ
#if ERROR == 4
  {
    // ERROR(4):Ubound
    int *f = &local;
    int *s = &local; s += (&global - s); // s is SEQ
    f ++; // f has type FSEQ
    if(! HAS_KIND(f, "FSEQ")) E(4);
    f = s; //Actually we fail here because s is below its home
    gptr = f; // Should fail
  }
#endif
  

  // Now writing structures
#if 5 <= ERROR && ERROR <= 7
  {
    static struct str1 {
      int i1;
      struct {
        int *s2;
      } i2;
      int * i3;
    } gstr;
    struct str1 res = { 0, &global, &global };//ERROR(5):Error 5
    struct str1 res = { 0, &local, &global };//ERROR(6):Stack address
    struct str1 res = { 0, &global, &local };//ERROR(7):Stack address
    gstr = res;
    E(5);
  }
#endif

  // Now write an array
#if 8 <= ERROR && ERROR <= 10
  {
    static struct strarr {
      int *a[4];
    } garr;
    struct strarr res = { &global, &global, &global };//ERROR(8):Error 8
    struct strarr res = { 0, &local, &global };//ERROR(9):Stack address
    struct strarr res = { 0, &global, &local };//ERROR(10):Stack address
    garr = res;
    E(8);
  }
#endif

  // Now write a union
#if 11 <= ERROR && ERROR <= 13
  {
    static union un {
      int *a[4];
      struct { int *a1, *a2, *a3; } b;
    } gun;
    union un res = { &global, &global, &global };//ERROR(11):Error 11
    union un res = { 0, &local, &global };//ERROR(12):Stack address
    union un res = { 0, &global, &local };//ERROR(13):Stack address
    gun = res;
    E(11);
  }
#endif

  
  
}





