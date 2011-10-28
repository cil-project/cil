#include "testharness.h"

#define MAXINT (1ull << ((8 * sizeof(int)) - 1))

int g1[ MAXINT / sizeof(int)  ];
char g2[ MAXINT / sizeof(char) ];
double g3[ MAXINT / sizeof(double) ];

typedef int g4[ MAXINT / sizeof(int) ];
typedef char g5[ MAXINT / sizeof(char) ];
typedef double g6[ MAXINT / sizeof(double) ];

struct cmsghdr {
    int cmsg_type;

    __extension__ unsigned char __cmsg_data [];

  };

void os_rcv_fd()
{
        char buf[sizeof(struct cmsghdr)];

        buf[0] = 0;
}

int main() {
  g4 *p1 = &g1;
  g5 *p2 = &g2;
  g6 *p3 = &g3;

  g1[MAXINT / sizeof(int) - 1] = 1;
  g2[MAXINT / sizeof(char) - 1] = 2;
  g3[MAXINT / sizeof(double) - 1] = 3;

  if(*p1[MAXINT / sizeof(int) - 1] != 1) E(1);
  if(*p2[MAXINT / sizeof(char) - 1] != 2) E(2);
  if(*p3[MAXINT / sizeof(double) - 1] != 3) E(3);

  return 0;
}
