
static char *usageplocal = "Usage";
static char usageescape = 'C';

char *usagep = "Usage non-local";
char usages[] = "Usage string";

char strange[] = { 'a', 'b', "several", 'c', "a quote\"" };

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


typedef struct {
  struct {
    char * a1[10];
    char * a2;
    char   strbuff[20] NULLTERM;
  } f1;
  struct {
    int * i1;
  } f2[5] SIZED;
} NESTED;

NESTED glob1;

int glob3;
int * glob2 = & glob3;

int afunc(void) {
  NESTED loc1;
  char   locbuff[30] NULLTERM;
  char   indexbuff[10] SIZED;

  loc1.f1.a2 = glob1.f1.a2;
  
  return * loc1.f2[3].i1 + (locbuff[0] - indexbuff[0]);
}
