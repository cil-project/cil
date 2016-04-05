#include <stdlib.h>

int f(void)
{
	return 42;
}

int g(void) __attribute((alias("f")));

int main(void)
{
	if (f == g) return g() - 42;
	else abort();
}
