#include "testharness.h"
#include <stddef.h>

/* Built-in Function: size_t __builtin_object_size (void * ptr, int type)
 *     is a built-in construct that returns a constant number of bytes
 *     from ptr to the end of the object ptr pointer points to (if known
 *     at compile time). __builtin_object_size never evaluates its
 *     arguments for side-effects. If there are any side-effects in
 *     them, it returns (size_t) -1 for type 0 or 1 and (size_t) 0 for
 *     type 2 or 3. If there are multiple objects ptr can point to and
 *     all of them are known at compile time, the returned number is the
 *     maximum of remaining byte counts in those objects if type & 2 is
 *     0 and minimum if nonzero. If it is not possible to determine
 *     which objects ptr points to at compile time,
 *     __builtin_object_size should return (size_t) -1 for type 0 or 1
 *     and (size_t) 0 for type 2 or 3.
 *
 *    type is an integer constant from 0 to 3. If the least significant
 *    bit is clear, objects are whole variables, if it is set, a closest
 *    surrounding subobject is considered the object a pointer points
 *    to. The second bit determines if maximum or minimum of remaining
 *    bytes is computed.
 *
 *  -- GCC manual, section 6.52 "Object Size Checking Builtins"
 *  */

int main() {
    struct V { char buf1[10]; int b; char buf2[10]; } var;
    char *p = &var.buf1[1], *q = (char *)&var.b;
    int i = 0; char b[2];

    /* Here the object p points to is var.  */
    if (__builtin_object_size (p, 0) != sizeof (var) - 1)
        E(1);
    /* The subobject p points to is var.buf1.  */
    if (__builtin_object_size (p, 1) != sizeof (var.buf1) - 1)
        E(2);
    /* The object q points to is var.  */
    if (__builtin_object_size (q, 0)
            != (char *) (&var + 1) - (char *) &var.b)
        E(3);
    /* The subobject q points to is var.b.  */
    if (__builtin_object_size (q, 1) != sizeof (var.b))
        E(4);

    /* Side-effects */
    if (__builtin_object_size (b + (++i),0) != (size_t)-1)
        E(5);
    if (__builtin_object_size (b + (++i),1) != (size_t)-1)
        E(6);
    if (__builtin_object_size (b + (++i),2) != (size_t)0)
        E(7);
    if (__builtin_object_size (b + (++i),3) != (size_t)0)
        E(8);
    if (i != 0)
        E(9);
    return 0;
}
