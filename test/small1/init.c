
static char *usageplocal = "Usage";
static char usageescape = 'C';
char *usagep = "Usage non-local";
char usages[] = "Usage string";

char *null = (void*)0;

typedef struct s {
  char *name;
  int   data;
} STR;

extern int foo(int x);
int (*fptr)(int) = foo;

STR a[] = {
  {"first", 0},
  {"second", 1},
  {& usages[2], 2},
  { & usageescape, 3},
  { usages, 4},
};

