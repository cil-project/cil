// mergeinline2.c
// counterpart to mergeinline1.c

// prototype
static long *fill();

// call
int bar()
{
  long *w = fill();
  return (int)(*w);
}

