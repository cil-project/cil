# 1 "cilcode.tmp/ex17.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex17.cil.c"
# 1 "cilcode.tmp/ex17.c"
int main(void)
{
  int x ;
  int y ;
  int tmp ;
  int z ;
  int tmp___0 ;

  {
# 2 "cilcode.tmp/ex17.c"
  if (x) {
# 2 "cilcode.tmp/ex17.c"
    tmp = 2;
  } else {
# 2 "cilcode.tmp/ex17.c"
    tmp = 4;
  }
# 2 "cilcode.tmp/ex17.c"
  y = tmp;
# 3 "cilcode.tmp/ex17.c"
  if (x) {
# 3 "cilcode.tmp/ex17.c"
    tmp___0 = 1;
  } else
# 3 "cilcode.tmp/ex17.c"
  if (y) {
# 3 "cilcode.tmp/ex17.c"
    tmp___0 = 1;
  } else {
# 3 "cilcode.tmp/ex17.c"
    tmp___0 = 0;
  }
# 3 "cilcode.tmp/ex17.c"
  z = tmp___0;
# 5 "cilcode.tmp/ex17.c"
  if (x) {
# 5 "cilcode.tmp/ex17.c"
    if (y) {
# 5 "cilcode.tmp/ex17.c"
      return (0);
    } else {
# 5 "cilcode.tmp/ex17.c"
      return (1);
    }
  } else {
# 5 "cilcode.tmp/ex17.c"
    return (1);
  }
# 8 "cilcode.tmp/ex17.c"
  if (x) {
# 8 "cilcode.tmp/ex17.c"
    if (y) {
# 8 "cilcode.tmp/ex17.c"
      goto _L;
    } else {
# 8 "cilcode.tmp/ex17.c"
      goto _L___0;
    }
  } else
  _L___0:
# 8 "cilcode.tmp/ex17.c"
  if (z) {
    _L:
# 8 "cilcode.tmp/ex17.c"
    x ++;
# 8 "cilcode.tmp/ex17.c"
    y ++;
# 8 "cilcode.tmp/ex17.c"
    z ++;
# 8 "cilcode.tmp/ex17.c"
    x ++;
# 8 "cilcode.tmp/ex17.c"
    y ++;
# 8 "cilcode.tmp/ex17.c"
    return (z);
  }
}
}
