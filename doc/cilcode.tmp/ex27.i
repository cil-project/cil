# 1 "cilcode.tmp/ex27.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex27.c"
  int foo(int x, int y) {
    int unknown;
    if (unknown)
      return y + 2;
    return x + 3;
  }

  int bar(void) {
    return -1;
  }

  int main(void) {
    int a, b, c;
    a = foo(5, 7) + foo(6, 7) + bar();
    b = 4;
    c = b * b;
    if (b > c)
      return b - c;
    else
      return b + c;
  }
