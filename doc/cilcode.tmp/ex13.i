# 1 "cilcode.tmp/ex13.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex13.c"
  int x = 5;
  int main() {
    int x = 6;
    {
      int x = 7;
      return x;
    }
    return x;
  }
