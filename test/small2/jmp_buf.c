typedef struct
  {
    unsigned long int __val[(1024 / (8 * sizeof (unsigned long int))) ];
  } __sigset_t;

__sigset_t env;

int main() { return 0; } 

#if 0
/* 
 * Actual C code that generates this error ...
 */

#include <setjmp.h>
jmp_buf env;

int main()
{
    int status;

    if (status=setjmp(env)) {
	printf("Case 1.\n");
    } else {
	printf("Case 2.\n");
    }
    return 0;
}
#endif
