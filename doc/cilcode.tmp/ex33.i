# 1 "cilcode.tmp/ex33.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex33.c"
int (*pf)(void);
int f(void) {

   pf = &f;
   pf = ***f;
   pf();
   (****pf)();
   (***************f)();
}
