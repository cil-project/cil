#include <stdalign.h>

struct testalign {
  int f1;
} __attribute__((__aligned__(16)));

struct testalign2 {
  _Alignas(16) int f1;
};

struct testalign3 {
  _Alignas(long double) int f1;
};


int main() {
  struct testalign a;
  char static_assertion_failure_1[(__alignof__ (a) == 16) ? 1 : -1];

  struct testalign2 b;
  char static_assertion_failure_2[(__alignof__ (b) == 16) ? 1 : -1];

  struct testalign3 c;
  char static_assertion_failure_3[(__alignof__ (c) == __alignof__ (long double)) ? 1 : -1];
  return 0;
}
