// unscomp.c
// show problem with linux/fs/buffer.c and unsigned comparisons
                                    
#include <stdio.h>    // printf
int main()
{
  unsigned long size;
  long offset;

  size = 1024;
  offset = 50;

  if ((offset -= size) >= 0) { 
    // 50 - 1024 is negative
    printf("no -- this is wrong\n");
    return 2;
  }
  else {
    printf("yes this is right\n");
  }
  
  return 0;
}
