// tagfile2.c
// second half of a tagfile tester

// external decl of function in tagfile1.c
int foo(int x);

typedef void (*voidFn)();

int main()
{
  voidFn tagMaker;
  int x;

  tagMaker = (voidFn)&foo;      // make CCured tag 'foo'
  x = foo(3);                   // but call it normally
  
  return x-10;                  // should be 0
}



