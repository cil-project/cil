// two static variables with the same name

#include <assert.h>

int foo()
{
  static int x = 0;
  return x;
}

int bar()
{
  static int x = 5;
  return x;
}

int main()
{
  assert(foo() + bar() == 5);
  return 0;
}

