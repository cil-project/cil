# 1 "cilcode.tmp/ex19.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex19.cil.c"
# 1 "cilcode.tmp/ex19.c"
int main(void)
{
  int x ;
  int i ;

  {
# 2 "cilcode.tmp/ex19.c"
  i = 0;
# 2 "cilcode.tmp/ex19.c"
  while (i < 5) {
# 3 "cilcode.tmp/ex19.c"
    if (i == 5) {
# 3 "cilcode.tmp/ex19.c"
      goto __Cont;
    }
# 4 "cilcode.tmp/ex19.c"
    if (i == 4) {
# 4 "cilcode.tmp/ex19.c"
      break;
    }
# 5 "cilcode.tmp/ex19.c"
    i += 2;
    __Cont:
# 2 "cilcode.tmp/ex19.c"
    i ++;
  }
# 7 "cilcode.tmp/ex19.c"
  while (x < 5) {
# 8 "cilcode.tmp/ex19.c"
    if (x == 3) {
# 8 "cilcode.tmp/ex19.c"
      continue;
    }
# 9 "cilcode.tmp/ex19.c"
    x ++;
  }
# 11 "cilcode.tmp/ex19.c"
  return (0);
}
}
