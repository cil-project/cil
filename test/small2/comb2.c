// com2.c
// part 1 of a program expected to be combined

#include <stdio.h>     // printf

int global_com2 = 5;

int foo_com2(int x)
{
  return x + global_com2;
}

extern int foo_com1(int x);
extern void hpfy();

int main()
{
  printf("foo_com1(6): %d\n", foo_com1(6));
  printf("foo_com2(61): %d\n", foo_com2(61));
  hpfy();
  return 0;
}
