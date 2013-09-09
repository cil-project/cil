#include "testharness.h"

struct s { unsigned long x; int y; char t[]; };
int main(){
  struct s a =
    { .x = sizeof(a),
      .y = 2,
    }
  ;
  static struct s b =
    { .x = sizeof(b),
      .y = 2,
      .t = {1,2,3}
    }
  ;
  /* the previous length of t is 3 - check that we do not truncate to 3
   * here as well. */
  static struct s c =
    { .x = sizeof(b),
      .y = 2,
      .t = {1,2,3,4,5}
    }
  ;


  if(a.x != sizeof(struct s)) E(1);
  if(b.t[2] != 3) E(2);
  if(c.t[4] != 5) E(2);

  SUCCESS;
}
