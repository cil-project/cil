#include <stdio.h>
#include <stdlib.h>

void deputy_fail(const char *what, const char *check,
                 const char *file, int line) {
    printf("Assertion \"%s\" failed in %s check at %s:%d.\n", 
          what, check, file, line);
    exit(1);
}

int deputy_strlen(const char *str) {
    return strlen(str);
}
