
#include "testharness.h"


typedef struct foo {
  struct foo *next;
  int *data;
} S;

S array[2];

S *fseq;

int main() {
  int data;
  fseq = array;
  array[1].next = & array[0];
  fseq ++;
  
  { __NOBOXBLOCK
      data = *(int*)fseq; // We don't want this cast to polute fseq
  }

  SUCCESS;
   
}
