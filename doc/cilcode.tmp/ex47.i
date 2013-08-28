# 1 "cilcode.tmp/ex47.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex47.c"
    static int bar(int x, char y) {
      return x + y;
    }


    int foo(int x, char y) __attribute__((alias("bar")));
