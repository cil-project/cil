extern int printf(char *, ...);
extern void exit(int);

#define E(n) { printf("Error %d\n", n); return n; }
#define SUCCESS { printf("Success\n"); return 0; }
