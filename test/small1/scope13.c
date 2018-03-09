/* Note how the statics each have the same name as a formal parameter,
 * hiding it. This has consequences for the alpha table scope handling. */
#include <stdlib.h>

static int
b(int *p)
{
	{
		static int p[] = { 41 };
		return p[0];
	}
}
int
c(int *p)
{
	{
		static int p[] = { 42 };
		return b(p);
	}
}

int main(void)
{
	int x = 3;
	if (c(&x) != 41) abort();
	return 0;
}
