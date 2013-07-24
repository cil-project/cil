# 1 "cilcode.tmp/ex40.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex40.c"
int main(void) {
# 1 "cilcode.tmp/ex40.c"
  int x, y, z;
  return &(x ? y : z) - & (x++, x);
}
