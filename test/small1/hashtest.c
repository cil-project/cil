

/* A special purpose main */
#include "main.h"
#include "hash.h"
#include "alloc.h"

/* Some globals that PCC needs */
int error_level, anerror;
void myexit(int n) {
  exit(n);
}
#ifdef _MSVC
#define random rand
#endif
extern int random(void);
int __mmId;
int debugMM;
int debug;




int main() {
  /* Test hash tables */
  PHASH h = NewHash();
  int i;
  double clk;
  
  /* Add and delete random numbers from the hash table */
  TIMESTART(clk);
  for(i=0;i<500000;i++) {
    int k = random();
    AddToHash(h, k, (void*)k);
  }
  TIMESTOP(clk);
  printf("Run hashtest in %8.3lfms\n", clk / 1000.0);
    
}
