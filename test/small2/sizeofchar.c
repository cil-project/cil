// sizeofchar.c
// from sac at stevechamberlain dot com

// problems with sizeof and chars
  
#include <assert.h>    // assert

int main()
{
  int sz1, sz2;

  sz1 = sizeof('a');         // 'a' is actually an int constant, so this is 4
  assert(sz1 == sizeof(int));
  
  sz2 = sizeof((char)'a');   // now it's 1!
  assert(sz2 == sizeof(char));
  
  return 0;
}

