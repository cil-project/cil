    static int bar(int x, char y) {
      return x + y;
    }

    //foo is considered another name for bar.
    int foo(int x, char y) __attribute__((alias("bar")));
