# 1 "cilcode.tmp/ex14.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex14.c"
  int x = 5;
  int main() {
    int x = 6;
    {
      static int x = 7;
      return x;
    }
    return x;
  }
