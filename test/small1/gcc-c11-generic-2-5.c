// https://github.com/gcc-mirror/gcc/blob/16e2427f50c208dfe07d07f18009969502c25dc8/gcc/testsuite/gcc.dg/c11-generic-2.c

void
f (int n)
{
  /* Type must be object type.  */
  _Generic (n, void (void): 0, default: 1);
}

int main() {
  return 0;
}
