// attempt to reproduce this error:
//  Failure at ./Config_Test.mergedcured.c:open__112ACE_Malloc_T__tm__92_21ACE_Local_Memory_Pool29ACE_Local_Memory_Pool_Options16ACE_Thread_Mutex17ACE_Control_BlockFv_i:161967: Creating an unaligned sequence

#include <stdlib.h>
#include <assert.h>

#define CCURED_TRUSTED_CAST(t, what) ((t)trusted_cast((void*)(what)))

int main() {
    struct s {
        int one; int two; int three;
    };
    char *p;
    struct s *s0;

    assert(sizeof (struct s)==12);
    p = malloc(15);
    s0 = CCURED_TRUSTED_CAST(struct s *, p);
    s0++;
    s0--;

    return 0;
}
