# 1 "cilcode.tmp/ex23.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex23.cil.c"
# 1 "cilcode.tmp/ex23.c"
int foo(int predicate )
{
  int x ;

  {
# 2 "cilcode.tmp/ex23.c"
  x = 0;
  {
# 4 "cilcode.tmp/ex23.c"
  if (predicate == 0) {
# 4 "cilcode.tmp/ex23.c"
    goto case_0;
  }
# 5 "cilcode.tmp/ex23.c"
  if (predicate == 1) {
# 5 "cilcode.tmp/ex23.c"
    goto case_1;
  }
# 6 "cilcode.tmp/ex23.c"
  if (predicate == 2) {
# 6 "cilcode.tmp/ex23.c"
    goto case_2;
  }
# 7 "cilcode.tmp/ex23.c"
  if (predicate == 3) {
# 7 "cilcode.tmp/ex23.c"
    goto case_3;
  }
# 8 "cilcode.tmp/ex23.c"
  goto switch_default;
  case_0:
# 4 "cilcode.tmp/ex23.c"
  return (111);
  case_1:
# 5 "cilcode.tmp/ex23.c"
  x ++;
  case_2:
# 6 "cilcode.tmp/ex23.c"
  return (x + 3);
  case_3:
# 7 "cilcode.tmp/ex23.c"
  goto switch_break;
  switch_default:
# 8 "cilcode.tmp/ex23.c"
  return (222);
  switch_break: ;
  }
# 10 "cilcode.tmp/ex23.c"
  return (333);
}
}
