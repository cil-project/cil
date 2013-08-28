#include "testharness.h"

// Variable-sized multidimensionnal arrays
void foo(int n, int a[n][n]);

int main(void)
{
  int a[40][40];
  foo(40, a);
  SUCCESS;
}

void foo(int n, int a[n][n]) {
  
   double b[n];
   a[0][n-1] = 0;
   b[n-1] = 0.0;
   printf("sizeof(a) = %d, sizeof(b) = %d\n", sizeof(a), sizeof(b));

   //formals should be promoted to pointers (int***, in this case)
   int (*p)[n] = a;
   p++;
   if (sizeof(a) != sizeof(p)) E(2);

   //locals should keep their array type.  CIL rewrites sizeof(b)
   // as (n * sizeof(*b))
   if (sizeof(b) != (n * sizeof(double))) E(3);
}

