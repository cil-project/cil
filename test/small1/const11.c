#include "testharness.h"

#ifdef _MSC_VER
#define uint64 unsigned __int64
#else
#define uint64 unsigned long long
#endif

int main () {
  int x = 257;
  int aa, bb, cc;
  //left shift
  int a = ((char)1) << 9 ;              // 512
  char b = ((char)1) << 9 ;             // 0
  int c = 1 << ((char)257);             // 2, since ((char)257) == 1
  uint64 d = 1 << 33 ;               // 0, since 1 is an int.
  uint64 e = ((uint64)1) << 33 ;  // 2,0000,0000h  (2**33)
  int f = 1 << 64;          // Don't fold this.
  int g = 1 << -1;          // Don't fold this.  treated as 0.


  printf("a=%d, b=%d, c=%d, d=%d:%u, e=%d:%u, f=%d, g=%d\n", 
         a, b, c, 
         (int)(d>>32), (unsigned int)d, (int)(e>>32), (unsigned int)e, 
         f, g);

  //right shift:
  aa = ((uint64)0x200000000LL) >> 33; // 1
  bb = ((uint64)0x200000000LL) >> 65; // Don't fold this.  gcc treats it as 0.
  cc = 3 >> -1;                        // Don't fold this. gcc treats it as 0.


  printf("aa=%d, bb=%d, cc=%d\n", 
         aa, bb, cc);

  if (a != 512) E(2);
  if (b != 0) E(3);
  if (c != 2) E(4);
  if (d != 0) E(5);
  if (e != ((uint64)0x200000000)) E(6);
  if (f != 0) E(7);
  if (g != 0) E(8);

  if (aa != 1) E(9);
  if (bb != 0) E(10);
  if (cc != 6) E(11);
  
  SUCCESS;
}
