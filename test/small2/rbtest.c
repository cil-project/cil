

/* A special purpose main */
#include "main.h"
#include "redblack.h"
#include "alloc.h"

/* Some globals that PCC needs */
int error_level, anerror;
void myexit(int n) {
  exit(n);
}
#ifdef _MSVC
#define random rand
#else
/* weimer: not needed: extern int random(void); */
#endif
int __mmId;
int debugMM;
int debug;


#define DATASIZE 16   // This is the size of the data that is reserved in
                      // each node

int main() {
  /* Test hash tables */
  RBNode *t = NULL;
  int i;
  double clk;
  int count = 0;
  int sz;
  
  /* Add and delete random numbers from the hash table */
  TIMESTART(clk);
  for(i=0;i<500000;i++) {
    int k = random() & 0x7FFFL;
    insertRB(& t, k, DATASIZE);
  }
  for(i=0;i<500000;i++) {
    int k = random() & 0x7FFFL;
    void *data = NULL;
    if(findRB(t, k)) {
      count ++;
    }
  }
  sz = 0;
  FORALLRBNODES(t, { sz ++; });
  freeRB(t, NULL);
  TIMESTOP(clk);
  fprintf(stderr, "Hash has %d elements. Found %d times\n",
          sz, count);
  printf("Run hashtest in %8.3lfms\n", clk / 1000.0);
  fprintf(stderr, "Hello\n");
  exit (0);
}


