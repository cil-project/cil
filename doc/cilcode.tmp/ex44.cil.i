# 1 "cilcode.tmp/ex44.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex44.cil.c"
# 1 "cilcode.tmp/ex44.c"
__inline int foo(void) ;
# 2 "cilcode.tmp/ex44.c"
int firstuse(void)
{
  int tmp ;

  {
# 2 "cilcode.tmp/ex44.c"
  tmp = foo();
# 2 "cilcode.tmp/ex44.c"
  return (tmp);
}
}
# 5 "cilcode.tmp/ex44.c"
__inline int foo(void)
{


  {
# 5 "cilcode.tmp/ex44.c"
  return (2);
}
}
# 7 "cilcode.tmp/ex44.c"
int main(void)
{
  int tmp ;
  int tmp___0 ;

  {
# 8 "cilcode.tmp/ex44.c"
  tmp = foo();
# 8 "cilcode.tmp/ex44.c"
  tmp___0 = firstuse();
# 8 "cilcode.tmp/ex44.c"
  return (tmp + tmp___0);
}
}
