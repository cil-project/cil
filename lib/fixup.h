/*** Macros to fixup some things that the parser does not understand ***/

#ifdef _MSVC   /************* MICROSOFT VISUAL C *************/
/* Drop some things on the floor */
#define __declspec(a)

/* Turn others into GCC syntax */
#define __inline inline
#define _inline inline
#define __int64 long long


#define random rand

/*
#include <time.h>
#define TIMESTART(clk) {clk=(double)clock();}
#define TIMESTOP(clk)  {clk=1000000.0 * \
                           ((double)clock()-(clk))/CLOCKS_PER_SEC;}
*/
#endif

/* Use this in the source to cast an integer what to a pointer in the same 
 * home area as host. Use this guarded by BEFOREBOX  */
/*#define CASTTOPOINTER(host, what) \
                     (((char*)(host)) + ((S32)(what) - (S32)(host))) */

#ifndef MANUALBOX
#define WILD
#define SAFE
#define TAGGED
#define INDEX
#define SIZED
#define SEQ
#define FSEQ
#define SEQN
#define FSEQN
#define NULLTERM
#define STRING
#else
#define WILD   __attribute__((wild))
#define SAFE   __attribute__((safe))
#define TAGGED __attribute__((tagged))
#define INDEX  __attribute__((index))
#define SIZED  __attribute__((sized))
#define SEQ    __attribute__((seq))
#define FSEQ   __attribute__((fseq))
#define SEQN   __attribute__((seqn))
#define FSEQN  __attribute__((fseqn))
#define NULLTERM   __attribute__((nullterm))
#define STRING  __attribute__((string))
#endif

#if ! defined(MANUALBOX) && ! defined(INFERBOX)
#define calloc_fseq calloc
#endif
