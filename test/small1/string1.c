#include "testharness.h"


#ifndef __RWSTRING
#define __RWSTRING
#define __FSEQN
#endif

int main() {
  char * __RWSTRING p = "";  // A pointer to an empty string
  char * __FSEQN pp;
  
  // Overwrite the zero
  *p = '1';

  // Now convert it to a FSEQ. Will call strlen which will fail
  pp = (char * __FSEQN)p;

  pp ++; // This should go outside of the string
  *pp = 0; // Bang

  SUCCESS;
  
}
