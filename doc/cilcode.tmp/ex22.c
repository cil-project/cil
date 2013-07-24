#include <stdio.h>

typedef int unused_type;

static char unused_static (void) { return 0; }

int main() {
  int unused_local;
  printf("Hello world\n"); // Only printf will be kept from stdio.h     
}
