// com1.c
// part 1 of a program expected to be combined

int global_com1 = 5;

int foo_com1(int x)
{
  return x + global_com1 + sizeof(int*);
}
  
int *globalPtr;

void hpfy()
{
  int local __HEAPIFY;
  globalPtr = &local;
}

int foo2_com1(int x)
{
  return x + global_com1 + sizeof(int*);
}

