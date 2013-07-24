# 1 "cilcode.tmp/ex33.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex33.cil.c"
# 1 "cilcode.tmp/ex33.c"
int (*pf)(void) ;
# 2 "cilcode.tmp/ex33.c"
int f(void)
{


  {
# 4 "cilcode.tmp/ex33.c"
  pf = & f;
# 5 "cilcode.tmp/ex33.c"
  pf = & f;
# 6 "cilcode.tmp/ex33.c"
  (*pf)();
# 7 "cilcode.tmp/ex33.c"
  (*pf)();
# 8 "cilcode.tmp/ex33.c"
  f();
# 9 "cilcode.tmp/ex33.c"
  return (0);
}
}
