// hola.c
// tiny program for scott's testing purposes



#define _GNU_SOURCE







struct __main_h_line_14 {};   // sm: testing
#include <stdio.h>
struct __main_h_line_16 {};   // sm: testing



// avoid pollution from headers: declare it myself
int printf(char const *fmt, ...);
void *malloc(unsigned int size);

#if 0
// try to recreate the unused tmps
typedef struct {
  char *ptr;
  char *end;
} FILE;
int __uflow(FILE *fp);
int getc_unlocked (FILE *fp)
{
//  return  fp->ptr >= fp->end;
  return  fp->ptr >= fp->end ?  __uflow(fp)
                             :  * ((unsigned char *)( fp->ptr++ ));
}
#endif // 0



#if 0
extern __inline  int
vprintf (const char *   __fmt)
{
  return 3;
}

extern __inline  int
vprintf (const char *   __fmt)
{
  return 4;
}
#endif // 0





// this is a pointer in the global area
int *globalPtr;

// here is a global integer to point at
int globalInt = 5;

               
// something so I can tell one section from another
void separator() {}


int main()
{
  // here's a local int to point at
  int localInt;
  
  // point at them
  globalPtr = &globalInt;

  separator();

  // this simply isn't allowed!
  //globalPtr = &localInt;
  
  separator();
  
  globalPtr = (int*)malloc(sizeof(int));

  printf("hola finished successfully\n");
  return 0;


  #if 0
  int x,y;
  x = printf("hola senior.\n");
  x += printf("what is ascii for accented o and tilde n?\n");
  x++;
  printf("x = %d\n", x);
  y = printf("hmm\n");
  return x?0:x;
  #endif // 0
}

