
/* VA.C: The program below illustrates passing a variable
 * number of arguments using the following macros:
 *      va_start            va_arg              va_end
 *      va_list             va_dcl (UNIX only)
 */

#include <stdio.h>
#include <stdarg.h>
#include <varargs.h>
int average( va_list );
#include "testharness.h"

int main( void )
{
   /* Call with 3 integers (-1 is used as terminator). */
  if(average( 2, 3, 4, -1 ) != 3) E(1);
  if(average( 5, 7, 9, 11, -1 ) != 8) E(2);
  if(average( -1 ) != 0) E(3);

   SUCCESS;
}

/* Returns the average of a variable list of integers. */
int average( va_alist )
va_dcl
{
   int i, count, sum;
   va_list marker;

   va_start( marker );            /* Initialize variable arguments. */
   for( sum = count = 0; (i = va_arg( marker, int)) != -1; count++ )
      sum += i;
   va_end( marker );              /* Reset variable arguments.      */
   return( sum ? (sum / count) : 0 );
}


