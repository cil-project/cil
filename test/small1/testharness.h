extern int printf(const char * __ROSTRING format, ...);
extern void exit(int);

/* Always call E with a non-zero number */
#define E(n) { printf("Error %d\n", n); exit(n); }
#define SUCCESS { printf("Success\n"); exit(0); }

