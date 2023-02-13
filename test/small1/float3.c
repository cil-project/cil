#include <stdio.h>
#include <string.h>
int main() {
  _Float128 f = 0.0q;
  _Float128 g = 0.0F128;
  _Float128 h = 0.0f128;
  _Float128 i = 0.0Q;

  _Complex _Float128 f1 = 0.0qi;
  _Complex _Float128 g1 = 0.0iF128;
  _Complex _Float128 h1 = 0.0f128i;
  _Complex _Float128 i1 = 0.0iQ;
  return 0;
}
