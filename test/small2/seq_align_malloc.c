// attempt to reproduce this error:
//  Failure at ./Config_Test.mergedcured.c:open__112ACE_Malloc_T__tm__92_21ACE_Local_Memory_Pool29ACE_Local_Memory_Pool_Options16ACE_Thread_Mutex17ACE_Control_BlockFv_i:161967: Creating an unaligned sequence

#include <stdlib.h>

int main() {
    struct s {
        int one; int two; int three;
    } *p;
    void *v = malloc(15);
    p = (struct s*)v; // Check here that there is enough space in p
    p ++;
    p--;

    return 0;
}
