extern int printf(const char *, ...);
extern void exit(int);
#ifndef strcmp
extern int strcmp(const char*, const char*);
#endif

// Always call E with a non-zero number
#define E(n) { printf("Error %d\n", n); return n; }
#define SUCCESS { printf("Success\n"); return 0; }

int * safe;
int * wild;
int * fseq;
int * seq;

// Set the kinds of the above pointers
void setkinds(void) {
  wild = (int *)&wild;
  fseq ++;
  seq -= 2;
}


#ifdef CCURED
  // Get the kind of a pointer
  // Returns one of "WILD", "SAFE", "SEQ", "FSEQ", "INDEX"
  #define KIND_OF(p) ccured_kind_of(p)
  // Check that a pointer has an expected kind. We might be making everything
  // wild.
  #define CHECK_KIND(p, kind) \
     !strcmp(KIND_OF(p), \
             (sizeof(wild) == sizeof(safe) ? "WILD" : kind))
#else
  #define KIND_OF(p) "SAFE"
  #define CHECK_KIND(p, kind) 1
#endif

