#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

void f(s1, s2) char *s1, *s2;
{
  printf("%s\n", s1);
  printf("%s\n", s2);
}

int main(int argc, char** argv) {
  f("hello there", "wow");
  f("hello");
}

