

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

#define ITERS 500000                      
//#define ITERS 1

int main() {
  /* Test hash tables */
  RBNode *t = NULL;
  int i;
  double clk;
  int count = 0;
  int sz;

  /* Add and delete random numbers from the hash table */
  TIMESTART(clk);
  printf("inserting...\n");
  for(i=0;i<ITERS;i++) {
    int k = random() & 0x7FFFL;
    insertRB(& t, k, DATASIZE);
  }
  printf("finding...\n");
  for(i=0;i<ITERS;i++) {
    int k = random() & 0x7FFFL;
    void *data = NULL;
    if(findRB(t, k)) {
      count ++;
    }
  }
  sz = 0;
  printf("sz++...\n");
  FORALLRBNODES(t, { sz ++; });
  printf("freeing...\n");
  freeRB(t, NULL);
  TIMESTOP(clk);
  fprintf(stderr, "Hash has %d elements. Found %d times\n",
          sz, count);
  printf("Run rbtest in %8.3lfms\n", clk / 1000.0);
  exit (0);
  return 0;
}


