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
#ifdef BEFOREBOX
#define CASTTOPOINTER(btyp, host, what) \
      (((btyp *)(host)) + (((S32)(what) - (S32)(host)) / ((S32)sizeof(btyp)))) 
#else
#define CASTTOPOINTER(btyp, host, what) ((btyp *)(what))
#endif

#ifndef MANUALBOX
#define __WILD
#define __SAFE
#define __TAGGED
#define __INDEX
#define __SIZED
#define __SEQ
#define __FSEQ
#define __SEQN
#define __FSEQN
#define __NULLTERM
#define __STRING
#define __SAFEUNION
#else
#define __WILD   __attribute__((wild))
#define __SAFE   __attribute__((safe))
#define __TAGGED __attribute__((tagged))
#define __INDEX  __attribute__((index))
#define __SIZED  __attribute__((sized))
#define __SEQ    __attribute__((seq))
#define __FSEQ   __attribute__((fseq))
#define __SEQN   __attribute__((seqn))
#define __FSEQN  __attribute__((fseqn))
#define __NULLTERM   __attribute__((nullterm))
#define __STRING  __attribute__((string))
#define __SAFEUNION __attribute__((safeunion))
#endif

#if ! defined(MANUALBOX) && ! defined(INFERBOX)
#define calloc_fseq calloc
#endif


// Add some prototypes for the built in fucntions
#ifdef _MSVC
void exit(int);
#endif

#ifdef _GNUCC
void exit(int);
#endif
