#include <string.h>
#include <stdlib.h>

void *memmove(void *dest, const void *src, size_t n)
{
	exit(0);
}

int main(int argc, char **argv)
{
	__builtin_memmove(argv[0], argv[0] + 1, strlen(argv[0]));
	exit(1);
}
