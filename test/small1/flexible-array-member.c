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

  if(a.x != sizeof(struct s)) E(1);
  if(b.t[2] != 3) E(2);

  SUCCESS;
}
