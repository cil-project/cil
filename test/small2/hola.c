// hola.c
// tiny program for scott's testing purposes


// inline func that isn't used
inline int not_used() { return 4; }

// inline func that *is* used
inline int am_used() { return 6; }



// avoid pollution from headers: declare it myself
int printf(char const *fmt, ...);
void *malloc(unsigned int size);



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
  
  // call that inline func
  localInt = am_used();
  
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

