// constfold.c
// problem with ijpeg and power constant folding

#include <math.h>    // sqrt

int main()
{
  {
    float z10, z5, tmp12;
    tmp12 = (float )(- 2.613125930) * z10 + z5;     // ijpeg
  }

  {
    double a,b,c,root;
    root = (-b-sqrt(b*b-4*a*c))/(2*a);              // power
  }

  return 0;
}
