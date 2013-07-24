# 1 "cilcode.tmp/ex26.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex26.cil.c"
# 1 "cilcode.tmp/ex26.c"
int foo(int predicate )
{
  int __retres2 ;

  {
# 2 "cilcode.tmp/ex26.c"
  if (predicate <= 0) {
# 3 "cilcode.tmp/ex26.c"
    __retres2 = 1;
# 3 "cilcode.tmp/ex26.c"
    goto return_label;
  } else {
# 5 "cilcode.tmp/ex26.c"
    if (predicate > 5) {
# 6 "cilcode.tmp/ex26.c"
      __retres2 = 2;
# 6 "cilcode.tmp/ex26.c"
      goto return_label;
    }
# 7 "cilcode.tmp/ex26.c"
    __retres2 = 3;
# 7 "cilcode.tmp/ex26.c"
    goto return_label;
  }
  return_label:
# 1 "cilcode.tmp/ex26.c"
  return (__retres2);
}
}
