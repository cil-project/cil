extern int printf(const char *, ...);
extern void exit(int);

/* Always call E with a non-zero number */
#define E(n) { printf("Error %d\n", n); return n; }
#define SUCCESS { printf("Success\n"); return 0; }

