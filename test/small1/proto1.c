
int foo(); // Forward declaration

struct bar {
  int x, y;
};

int (*pfoo)() = foo;

// Now the real declaration
int foo(struct bar *a) {
  return a->x + a->y;
}

