// Makes sure that pointers within structures are checked.
// NUMERRORS 1

#include <malloc.h>

struct foo
{
    char * __FSEQ p;
};

int
bar(struct foo *fp)
{
    struct foo f;
    char buf[10];
    f.p = buf;
    *fp = f; // ERROR(1):Storing stack address
}

void
main(void)
{
    struct foo f;
    return bar(&f);
}
