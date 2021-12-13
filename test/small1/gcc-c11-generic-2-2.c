// https://github.com/gcc-mirror/gcc/blob/16e2427f50c208dfe07d07f18009969502c25dc8/gcc/testsuite/gcc.dg/c11-generic-2.c

void
f (int n)
{
  /* Variably-modified type not ok.  */
  _Generic (n, int[n]: 0, default: 1);
}

int main() {
  return 0;
}
