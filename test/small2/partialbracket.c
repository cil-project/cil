// partially-bracketed initializers cause problems

struct S {
  int x, y;
};

struct S array[] = {
  1,2,
  3,4
};

struct S array_ok[] = {
  {1,2},
  {3,4}
};
