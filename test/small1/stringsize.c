
#include "testharness.h"

#define A_STRING "a string literal for testing"
int main()
{
  char tmp[sizeof(A_STRING)] = A_STRING;

  //This fails because cabs2CIL gets the wrong value for sizeof(A_STRING)
  if( sizeof(tmp) != 30 )  E(1);

  //This fails on CCured only because markptr inserts a cast to char*
  if( sizeof("Hello, world.") != 14 )  E(2);

  //This fails because the CIL conversion drops the char* cast.
  if( sizeof((char*)"Hello, world.") != 4 )  E(3);

  SUCCESS;
}

