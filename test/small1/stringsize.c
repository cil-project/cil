//All three of these cases work in gcc, but fail in CIL.  The first case was
//a problem for OpenSSH, which uses something like the declaration of tmp
//below to initialize a string buffer


#include "testharness.h"

#define A_STRING "a string literal for testing."
int main()
{
  char tmp[sizeof(A_STRING)] = A_STRING;

  //This fails because cabs2CIL thinks sizeof(A_STRING) == 4,
  //so the array is not completely initialized.
  if( sizeof(tmp) != 30 )  E(1);
  if( tmp[10] != (A_STRING)[10] )  E(1);

  //This fails on CCured only because markptr inserts a cast to char*
  if( sizeof("Hello, world.") != 14 )  E(2);

  //This fails because the CIL conversion drops the char* cast.
  if( sizeof((char*)"Hello, world.") != 4 )  E(3);

  SUCCESS;
}

