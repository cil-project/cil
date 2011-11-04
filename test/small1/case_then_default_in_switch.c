// Running this file with --domakeCFG yields an incorrect result in CIL 1.3.7 (revision 12099).
#include "testharness.h"

int main() {
	switch (1) {
	case 0:
	default:
		break;
	case 1: ;
		SUCCESS;
	}
	E(0);
}
