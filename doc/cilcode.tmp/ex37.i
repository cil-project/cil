# 1 "cilcode.tmp/ex37.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex37.c"
  int foo() {
     static bar();
     static (*pbar)() = bar;

  }

  static bar() {
    return 1;
  }

  static (*pbar)() = 0;
