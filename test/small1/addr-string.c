#include "testharness.h"
#include <stddef.h>

int main() {

  // String literals are lvalues
  char (*p)[4] = &("bar");
  wchar_t (*q)[4] = &(L"foO");

  if((*p)[1] != 'a') E(1);
  if((*q)[1] != 'o') E(2);
  
  SUCCESS;
}
