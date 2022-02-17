#include<math.h>
#include "testharness.h"
double e = __builtin_nanf("")+1.0;
double d = NAN;

int main(void) {
	if (e == e) { E(1); }
	if (d == d) { E(2); }

	SUCCESS;
}
