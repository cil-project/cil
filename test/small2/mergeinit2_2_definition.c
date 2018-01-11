#include "mergeinit1.h"
#include "mergeinit2.h"
#include "mergeinit3.h"


int (*table[2])(void) =
{
      &f1,
      &f3
};
