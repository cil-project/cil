#include "../small1/testharness.h"

#include <excpt.h>

// NUMERRORS 1

// This is for MSVC
#ifndef _MSVC
error This test works only for MSVC
#endif

int throw() {
  // Simulate a segfault
  *((int*)0) = 5;
}

int main() {

  int i = 0;

  __try {
    i ++;
  } __finally {
    i --;
  }

  __try {
    i ++;
  } __except(EXCEPTION_EXECUTE_HANDLER) {
    i --;
  }
    
  if(i != 0) E(100);
  
  SUCCESS;
}
     

