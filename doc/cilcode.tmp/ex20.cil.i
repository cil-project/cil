# 1 "cilcode.tmp/ex20.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex20.cil.c"
# 1 "cilcode.tmp/ex20.c"
int main(void)
{
  int x ;
  int y ;
  int z ;

  {
# 1 "cilcode.tmp/ex20.c"
  x = 5;
# 1 "cilcode.tmp/ex20.c"
  y = x;
# 2 "cilcode.tmp/ex20.c"
  x ++;
  L:
# 2 "cilcode.tmp/ex20.c"
  y -= x;
# 2 "cilcode.tmp/ex20.c"
  z = y;
# 3 "cilcode.tmp/ex20.c"
  goto L;
# 3 "cilcode.tmp/ex20.c"
  return (0);
}
}
