typedef struct
  {
    unsigned long int __val[(1024 / (8 * sizeof (unsigned long int))) ];
  } __sigset_t;

__sigset_t env;

extern int setjmp(__sigset_t *);
extern int longjmp(__sigset_t *, int);
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
	    printf("Wrong status value!\n");
	}
    } else {
	printf("Saved state via setjmp, status = 0.\n");
    }
    if (status == 0) {
	printf("Long-Jumping with status argument 55.\n");
	longjmp(&env,55);
    }
    return 0;
}

