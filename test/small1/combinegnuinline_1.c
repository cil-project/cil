// https://github.com/goblint/cil/pull/85
#include <assert.h>
#include "combinegnuinline_header.h"
int main() {
    int x = goo();
    assert(x == 2);
    return 0;
}
