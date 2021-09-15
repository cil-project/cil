// https://github.com/gcc-mirror/gcc/blob/16e2427f50c208dfe07d07f18009969502c25dc8/gcc/testsuite/gcc.dg/c11-generic-2.c

void
f (int n)
{
  /* Two compatible types in association list.  */
  _Generic (&n, int: 5, signed int: 7, default: 23);
}

int main() {
  return 0;
}
