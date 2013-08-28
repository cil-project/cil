#include "testharness.h"
#pragma GCC diagnostic ignored "-Waddress"

struct s { _Bool s: 1; _Bool t; } s;

char a[1 == 1 ? 1 : -1];
char b[0 == 0 ? 1 : -1];
char c[1 == 1 ? 1 : -1];

char d[(_Bool) 0.5 == 1 ? 1 : -1];

_Bool e = &s;


char f[(_Bool) 0.0 == 0 ? 1 : -1];

char g[1];
char h[sizeof (_Bool)];

char i[sizeof s.t];

enum { j = 0, k = 1, l = 0 * 1, m = 1 * 256 };
_Bool n[m];
char o[sizeof n == m * sizeof n[0] ? 1 : -1];
char p[-1 - (_Bool) 0 < 0 && -1 - (_Bool) 0 < 0 ? 1 : -1];

_Bool q = 1;
_Bool *pq = &q;

int
main ()
{
  {
    _Bool e1 = &s;
    if (!e1)
      E(1);
  }
  {
    char digs[] = "0123456789";
    if (&(digs + 5)[-2 + (_Bool) 1] != &digs[4])
      E(2);
  }

  SUCCESS;
}
