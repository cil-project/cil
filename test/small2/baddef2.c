// baddef2.c: other def'n
  
#include <stdio.h>

struct S {
  int x;
  int y;
  int z;      // third field!
};

int size2() { return sizeof(struct S); }
int size1();  // from baddef1

int main()
{
  printf("size1: %d\n", size1());
  printf("size2: %d\n", size2());
  printf("(correct output is 8, then 12)\n");
  return 0;
}


