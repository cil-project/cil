#include <stdio.h>

#ifdef _GNUCC
#define LONGLONG long long
#define MODULE "GCC"
#endif

#ifdef _MSVC
#define LONGLONG __int64
#define MODULE "MSVC"
#endif

int main() {
  fprintf(stderr, "Generating machine dependency information for CIL\n");

  // Size of certain types
  printf("\tlet sizeof_int       = %d\n", sizeof(int));
  printf("\tlet sizeof_short     = %d\n", sizeof(short));
  printf("\tlet sizeof_long      = %d\n", sizeof(long));
  printf("\tlet sizeof_long      = %d\n", sizeof(long));
  printf("\tlet sizeof_longlong  = %d\n", sizeof(LONGLONG));
  printf("\tlet sizeof_ptr       = %d\n", sizeof(int *));
  printf("\tlet sizeof_enum      = %d\n", sizeof(enum { ONE, TWO }));
  printf("\tlet sizeof_longdouble  = %d\n", sizeof(long double));

  // The alignment of long long
  {
    struct longlong {
      char c;
      LONGLONG ll;
    };
    printf("\tlet alignof_longlong = %d\n",
           (int)(&((struct longlong*)0)->ll));
  }

  // The alignment of double
  {
    struct s1 {
      char c;
      double d;
    };
    printf("\tlet alignof_double = %d\n",
           (int)(&((struct s1*)0)->d));
  }    

  // The alignment of long  double
  {
    struct s1 {
      char c;
      long double ld;
    };
    printf("\tlet alignof_longdouble = %d\n",
           (int)(&((struct s1*)0)->ld));
  }    

  // The type of sizeof
  printf("\tlet ikind_sizeof_is_long = %s\n",
         sizeof(int) == sizeof(sizeof(long)) ? "true" : "false");

  // Whether char is unsigned
  printf("\tlet char_is_unsigned = %s\n", 
         ((char)0xff) > 0 ? "true" : "false");


  exit(0);
}
