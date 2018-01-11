// sizeofchar.c
// from sac at stevechamberlain dot com

// problems with sizeof and chars
  
#include <assert.h>    // assert
#include <limits.h>    // CHAR_MIN

int main()
{
  int sz1, sz2;
  int i;

  sz1 = sizeof('a');         // 'a' is actually an int constant, so this is 4
  assert(sz1 == sizeof(int));
  
  sz2 = sizeof((char)'a');   // now it's 1!
  assert(sz2 == sizeof(char));

  //character constants are ints, so this should be sign-extended to 0xFFFFFFFF
  i = '\xff';
  if(CHAR_MIN == 0) /* char is unsigned */
    assert((int)i == (int)255);
  else              /* char is signed */
    assert((int)i == (int)-1);

  {
    //Test CIL's understanding of '\xff' by forcing it to do constant folding:
    char array[(unsigned int)'\xff' - (unsigned int)0xfffff000];
    assert (sizeof(array) == 0xfff);
  }
  

  return 0;
}

