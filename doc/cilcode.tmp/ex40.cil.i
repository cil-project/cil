# 1 "cilcode.tmp/ex40.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex40.cil.c"
# 1 "cilcode.tmp/ex40.c"
int main(void)
{
  int x ;
  int y ;
  int z ;
  int *tmp ;

  {
# 2 "cilcode.tmp/ex40.c"
  if (x) {
# 2 "cilcode.tmp/ex40.c"
    tmp = & y;
  } else {
# 2 "cilcode.tmp/ex40.c"
    tmp = & z;
  }
# 2 "cilcode.tmp/ex40.c"
  x ++;
# 2 "cilcode.tmp/ex40.c"
  return ((int )(tmp - & x));
}
}
