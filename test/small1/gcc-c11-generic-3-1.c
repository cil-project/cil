// https://github.com/gcc-mirror/gcc/blob/16e2427f50c208dfe07d07f18009969502c25dc8/gcc/testsuite/gcc.dg/c11-generic-3.c

char const *a = _Generic ("bla", char *: "");
char const *c = _Generic ((int const) { 0 }, int: "");
char const *e = _Generic (+(int const) { 0 }, int: "");

int main() {
  return 0;
}
