#include "testharness.h"


int main()
{
  int x = 7;

  x = x++;
  /*
   * Strictly speaking, the order of increment versus assignment here
   * is not well defined: see ANSI C standard section 6.5.2.4 [Postfix
   * increment and decrement operators], paragraph 2.  However, both
   * GCC and VC do the assignment before the side effect.  For maximum
   * compatibility, then, the ending value of x should be 8, not 7.
   */

  if (x == 8)
    SUCCESS;
  else
    E(1);
}
