#include "stdio.h"
inline void add(int i, int j) {
  printf("Called inline\n");
}

int main() {
  add(4, 5);
  return 0;
}
