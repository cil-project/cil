# 1 "cilcode.tmp/ex12.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex12.cil.c"
# 2 "cilcode.tmp/ex12.c"
struct foo {
   int f1 ;
   int f2 ;
};
# 1 "cilcode.tmp/ex12.c"
int main(void)
{
  int x ;
  struct foo a[3] ;

  {
# 1 "cilcode.tmp/ex12.c"
  x = 5;
# 2 "cilcode.tmp/ex12.c"
  a[0].f1 = 1;
# 2 "cilcode.tmp/ex12.c"
  a[0].f2 = 2;
# 2 "cilcode.tmp/ex12.c"
  a[1].f1 = 3;
# 2 "cilcode.tmp/ex12.c"
  a[1].f2 = 4;
# 2 "cilcode.tmp/ex12.c"
  a[2].f1 = 5;
# 2 "cilcode.tmp/ex12.c"
  a[2].f2 = 0;
# 3 "cilcode.tmp/ex12.c"
  return (0);
}
}
