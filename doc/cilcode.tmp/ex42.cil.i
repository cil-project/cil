# 1 "cilcode.tmp/ex42.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex42.cil.c"
# 1 "cilcode.tmp/ex42.c"
static void *jtab[2] ;
# 4 "cilcode.tmp/ex42.c"
static int doit(int x ) ;
# 4 "cilcode.tmp/ex42.c"
static int jtab_init = 0;
# 2 "cilcode.tmp/ex42.c"
static int doit(int x )
{
  int __compgoto ;

  {
# 5 "cilcode.tmp/ex42.c"
  if (! jtab_init) {
# 6 "cilcode.tmp/ex42.c"
    jtab[0] = (void *)0;
# 7 "cilcode.tmp/ex42.c"
    jtab[1] = (void *)1;
# 8 "cilcode.tmp/ex42.c"
    jtab_init = 1;
  }
# 10 "cilcode.tmp/ex42.c"
  __compgoto = (int )jtab[x];
# 10 "cilcode.tmp/ex42.c"
  switch (__compgoto) {
  case 1:
# 10 "cilcode.tmp/ex42.c"
  goto lbl2;
  case 0:
# 10 "cilcode.tmp/ex42.c"
  goto lbl1;
  default:
# 10 "cilcode.tmp/ex42.c"
  *((int *)0) = 0;
  }
  lbl1:
# 12 "cilcode.tmp/ex42.c"
  return (0);
  lbl2:
# 14 "cilcode.tmp/ex42.c"
  return (1);
}
}
# 18 "cilcode.tmp/ex42.c"
extern int ( exit)() ;
# 17 "cilcode.tmp/ex42.c"
int main(void)
{
  int tmp ;
  int tmp___0 ;

  {
# 18 "cilcode.tmp/ex42.c"
  tmp = doit(0);
# 18 "cilcode.tmp/ex42.c"
  if (tmp != 0) {
# 18 "cilcode.tmp/ex42.c"
    exit(1);
  }
# 19 "cilcode.tmp/ex42.c"
  tmp___0 = doit(1);
# 19 "cilcode.tmp/ex42.c"
  if (tmp___0 != 1) {
# 19 "cilcode.tmp/ex42.c"
    exit(1);
  }
# 20 "cilcode.tmp/ex42.c"
  exit(0);
}
}
