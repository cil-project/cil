// jmp_buf.c
// another setjmp test

// sm: my setjmp_w wrapper needs this
#include <setjmp.h>     // jmp_buf

// may as well add this..
#include <stdio.h>      // printf

//extern void printf(char *, ...);
  
     
// sm: these conflict with glibc2 header; hopefully the setjmp.h
// #inclusion above will be sufficient for whichever platform
// this code was intended?
#if 0
  typedef struct
    {
      unsigned long int __val[(1024 / (8 * sizeof (unsigned long int))) ];
    } __sigset_t;

  __sigset_t env;

  extern int setjmp(__sigset_t *);
  extern int longjmp(__sigset_t *, int);
#else
  jmp_buf env;
#endif

/* 
 * Actual C code that generates this error ...
 * Correct output:
 * 	Saved state via setjmp, status = 0.
 * 	Long-Jumping with status argument 55.
 * 	Returned from longjmp, status = 55
 */

int main()
{
    int status;

    if (status=setjmp(&env)) {
	printf("Returned from longjmp, status = %d\n",status);
	if (status != 55) {
          printf("Wrong status value!\n"); return 1;
	}
    } else {
      printf("Saved state via setjmp, status = 0.\n");
    }
    if (status == 0) {
	printf("Long-Jumping with status argument 55.\n");
	longjmp(&env,55);
    }
    printf("success\n");
    return 0;
}

