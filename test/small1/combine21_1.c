static int gflag;

// This is not considered correct code in GNU11, but is ok in GNU90.
// -std=gnu90 used to be the default, but since GCC 5.1.0 it is -std=gnu11
// To account for this, -std=gnu90 was added to the regressions tests
// To make it GNU11, add extern
__inline void testit ( int flag )
{
        gflag = flag;
}

extern void otest(int flag);

int
main(int argc, char **argv)
{
        testit(0);
        otest(2);
        testit(1);
        return 0;
}
