// hola.c
// tiny program for scott's testing purposes

// avoid pollution from headers: declare it myself
int printf(char const *fmt, ...);                 

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


int main()
{ 
  int x,y;
  x = printf("hola senior.\n");
  x += printf("what is ascii for accented o and tilde n?\n");
  x++;
  printf("x = %d\n", x);
  y = printf("hmm\n");
  return x?0:x;
}

