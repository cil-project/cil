// combine_samefn_1.c
// test merging two source files, each of which contain a definition
// of a given function, but those definitions are identical

// repeated function
int foo(int x)
{
  int y = x + 3;    // 8
  int z = y + x;    // 13
  return z + x;     // 18
}


int myglobal;


int main()
{
  return foo(5) - 18;
}
