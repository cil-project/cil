
//copied from deepcopy2.c

#ifndef __SEQ
 #define __SEQ
 #define __FSEQ
#endif


#define NR_BARS 4
// A test with a deep-copy
struct foo {
  int              an_int;
  double           a_double;
  char * __SEQ     a_string;
  double  * __FSEQ an_array;       // Length is below
  int              an_array_count; // The length of the above array
  struct bar * __FSEQ bars;      // Array of length NR_BARS
};

struct bar {
  char * __SEQ * __FSEQ  argv; // Null-terminated array of strings
};

// Construct a few BARs
static char * strings1[] = { "one_1", "two_1", "three_1", 0 };
static char * strings2[] = { "one_2", "two_2", "three_2", 0 };
static char * strings3[] = { "one_3", "two_3", "three_3", 0 };
static char * strings4[] = { 0 };
static struct bar arrbar[NR_BARS] = { strings1, strings2, strings3, strings4 };


// Construct something of type foo
static double arr[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };
static struct foo x = { 5, 6.0, "a string",
                 arr, sizeof(arr) / sizeof(double),
                 arrbar };

void* external(void * arg) { return arg; }
void* true_external(void * arg) { return arg; }

void* external_noarg(void) { return &x; }
void* true_external_noarg(void) { return &x; }

