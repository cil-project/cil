# 1 "cilcode.tmp/ex32.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex32.c"
int x = 5;
int f() {
  int x = 3;
  {
    extern int x;
    return x;
  }
}
