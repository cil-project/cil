// mergeinline1.c
// hypothesis about fill_n_... problem

// prototype
static long *fill();

// call
int foo()
{
  long *w = fill();
  return (int)(*w);
}

// inline definition
__inline static long *fill()
{
  return 0;
}

int main()
{
  return 0;
}
