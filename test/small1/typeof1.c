/* This test case was reported by Robert. It seems that CIL ought to 
 * understand that typeof is an abbreviation */

#ifdef __GNUC__
void foo();


__typeof(foo) a; // A prototype
void a() {}

void b(void); // A prototype for b
extern __typeof(a) b __attribute__ ((alias ("a"))); // And another

// In GCC we can take the sizeof a function. It will be 1
int x = sizeof(foo);

int arr[8];

__typeof(arr) barr = { 0, 1, 2, 3 } ;

#endif


typedef int FUN(int);

FUN fptr; // fptr is defined to be a function! This is a prototype.

FUN fptr; // This is another prototype

int fptr(int x); // Yet another prototype

int fptr(int x) { // Now another definition for it
  return x - 1;
}

typedef int ARRAY[8];

ARRAY carr;

#ifdef __GNUC__
int y = sizeof(fptr);
#endif

int main(void) 
{
#ifdef __GNUC__ 
  a();
  b();
  if(x != 1) {
    exit(1);
  }
  if(sizeof(barr) != sizeof(arr)) {
    exit(2);
  }
  if(barr[2] != 2) {
    exit(3);
  }
#endif
  if(sizeof(carr) != sizeof(ARRAY)) {
    exit(4);
  }
  return fptr(1);
}
