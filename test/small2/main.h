/*  
 * Copyright (c) 1999 Cedilla Systems Incorporated.  All Rights Reserved.
 *
 * This software is the confidential and proprietary information of
 * Cedilla Systems Incorporated. ("Confidential Information").  You shall
 * not disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Cedilla Systems.
 * 
 *****************************************************************************/

/* weimer hack! */
//#define __qaddr_t	wes__qaddr_t
//#define __fsid_t	wes__fsid_t
//#define __caddr_t	wes__caddr_t
//#define __fd_set	wes__fd_set

#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
    
			      /* System includes and prototype for alpha_OSF */
#ifdef  alpha_UNIX
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#ifdef __GNUCC
struct timezone {
	int	tz_minuteswest;	/* minutes west of Greenwich */
	int	tz_dsttime;	/* type of dst correction */
};
#endif /* __GNUCC */
#include <sys/resource.h>
 
char*   strdup(const char*);
char*   strncpy(char*, const char*, size_t);
char*   strcpy(char*, const char*);
char*   strcat(char*, const char*);
int     strcmp(const char*, const char*);
void*   malloc(size_t);
void*   calloc(size_t, size_t);
void    bzero(char*, int);
void    free(void*);
void*   memmove(void*, const void*, unsigned long);
#ifdef  __GNUCC

/* long     read(int, void*, unsigned long);
   long     write(int, const void*, size_t);
   unsigned long  strlen(const char*);
   void*    memset(void*, int, unsigned long);
   void*   memcpy(void*, const void*, unsigned  long); */
#else
int     read(int, void*, unsigned long);
int     write(int, const void*, size_t);
int     strlen(const char*); 
char*   memset(void*, int, unsigned long);
char*   memcpy(void*, const void*, unsigned  long);
#endif
long    lseek(int, long, int);
long    tell(int);
int     close(int);
void    exit(int);

#endif  /* alpha_UNIX */

extern int* breakAddr;  /* Use *breakAddr = 0; to set a breakpoint */


#ifdef  x86_WIN32
#include <stdlib.h>
#include <string.h>
#include <io.h>
#include <time.h>

#ifdef  _GNUCC
#include <sys/unistd.h>
#endif  /* _GNUCC */

#ifdef  _MSVC
#define snprintf _snprintf
#define alloca       _alloca
#endif  /* _MSVC */

#endif  /* x86_WIN32 */

#ifdef  x86_LINUX
#include <stdlib.h>
#include <fcntl.h>
/* char *strdup(const char*); */
#include <sys/unistd.h>
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>

#endif  /* x86_LINUX */

#define USE_MODULO_ARITH_NO
#define DONT_USE_E

#define OCCURS_CHECK          /* If not defined it does not do the 
                               * occurs-check, which might lead to 
                               * unsoundness. Undefine only for experiments */
#define OCCURS_CHECK_OPT      /* If defined it enables the occurs-check 
                               * optimization */

#define MEMORY_OPT            /* If defined it enables the memory usage 
                               * optimization */

#define OLD_SREGS


 
extern int debug, debugSfpol;
extern int dummy; /* For debugging only */

#define SAFESTRCPY(dest,destlen, src, srclen) {\
                   char const *_ps = src;\
                   char *_pd = dest;\
                   int  _len = MIN((destlen), (srclen));\
                   while(*_ps && _len > 0) { *_pd++ = *_ps++; _len --;}\
                   if(_len > 0) {\
                     *_pd = '\0';\
                   } else {\
                     ERROR1(0, "String too long: %.1024s", src);\
                   }}

#define CHECKSTRING(str,end) {\
                   char const *_ps = str;\
                   char const *_pe = end;\
                   while(*_ps && _ps < _pe) {_ps ++;}\
                   if(_ps >= _pe) {\
                     ERROR1(0, "String too long: %.1024s", str);\
                   }}

#define SAFESTRCMP(dest, destlen, src, srclen) \
                   strncmp((dest),(src), MIN((destlen), (srclen)))

#define MIN(x,y) ((x)<(y)?(x):(y))
#define MAX(x,y) ((x)>(y)?(x):(y))
#define ABSVAL(x)   (((S32)(x)) >= 0 ? ((S32)(x)) : (- ((S32)(x))))

#define SETDBGOUT   int _stdout; fflush(stdout);_stdout=dup(1);dup2(2,1);
#define RESTOREOUT  fflush(stdout); dup2(_stdout, 1); close(_stdout);

#ifdef _DEBUG
#define IFDEBUG(txt) {if(debug) {SETDBGOUT; txt; RESTOREOUT;}}
#define IFDEBUG2(txt) {if(debug >= 2) {SETDBGOUT; txt; RESTOREOUT;}}
#define IFDEBUG3(txt) {if(debug >= 3) {SETDBGOUT; txt; RESTOREOUT;}}
#define IFTRUE(txt) {txt}
#ifdef _MSVC
#include <crtdbg.h>
#endif /* _MSVC */
#else  /* _DEBUG */
#define IFDEBUG(txt) {;}
#define IFDEBUG2(txt) {;}
#define IFDEBUG3(txt) {;}
#define IFTRUE(txt) {;}
#endif  /* _DEBUG */

void    myexit(int);

			      /* Error reporting */
#ifndef EMBEDDED
#define ERROR_LEVELS
#define EXIT_ON_ERROR
#endif

#ifdef  ERROR_LEVELS
extern  int error_level;
extern  int anerror;

#define EXIT(v, n)      {if(error_level) {error_level--;return v;}\
		         else {myexit(n);}}
#define ERR_SET          {anerror=1;}
#define ERR_CHECK(v,stop,prg) {if(anerror) {if(error_level) {error_level--;\
				 			     return v;}\
                                            else {prg;if(stop)myexit(1);\
						  return v;}}}				       
#else /* ERROR_LEVELS */

#define ERR_SET         
#define ERR_CHECK(v,stop,prg) 
#define EXIT(v, n)      exit(n)
#endif /* ERROR_LEVELS */


#ifndef EMBEDDED
#define ERRLINE    {printf( "Error at %s(%d):",__FILE__,__LINE__);\
		    ERR_SET;}
#define WARNLINE   {printf( "Warning at %s(%d):",__FILE__,__LINE__);}

#define ERROR0(v, txt)              {ERRLINE;\
			  	     printf(txt);EXIT(v,1);}
#define ERROR1(v, txt, d1)          {ERRLINE;\
				     printf(txt, d1);EXIT(v,1);}
#define ERROR2(v, txt, d1, d2)      {ERRLINE;\
				     printf(txt, d1, d2);EXIT(v,1);}
#define ERROR3(v, txt, d1, d2, d3)  {ERRLINE;\
				    printf(txt, d1, d2, d3);EXIT(v,1);}
#define ERROR4(v, txt, d1, d2, d3, d4)  \
                               {ERRLINE;\
				printf(txt, d1, d2, d3, d4);EXIT(v,1);}
#define ERROR5(v, txt, d1, d2, d3, d4, d5)  \
                               {ERRLINE;\
				printf(txt, d1, d2, d3, d4, d5);\
                                EXIT(v,1);}
#define ERROR6(v, txt, d1, d2, d3, d4, d5, d6)  \
                               {ERRLINE;\
				printf(txt, d1, d2, d3, d4, d5, d6);\
                                EXIT(v,1);}

#define WARNING0(txt)               {WARNLINE;\
                                     printf(txt);}
#define WARNING1(txt, d1)           {WARNLINE;\
                                     printf(txt, d1);}
#define WARNING2(txt, d1, d2)       {WARNLINE;\
                                     printf(txt, d1, d2);}
#define WARNING3(txt, d1, d2, d3)   {WARNLINE;\
                                     printf(txt, d1, d2, d3);}
#define WARNING4(txt, d1, d2, d3, d4)   {WARNLINE;\
                                  printf(txt, d1, d2, d3, d4);}
#define WARNING5(txt, d1, d2, d3, d4, d5)   {WARNLINE;\
                                  printf(txt, d1, d2, d3, d4, d5);}
#define WARNING6(txt, d1, d2, d3, d4, d5, d6)   {WARNLINE;\
                                  printf(txt, d1, d2, d3, d4, d5, d6);}
#else

#define ERROR0(v,txt)                     { myexit(1 + __LINE__); }
#define ERROR1(v,txt,d1)                  { myexit(1 + __LINE__); }
#define ERROR2(v,txt,d1,d2)               { myexit(1 + __LINE__); }
#define ERROR3(v,txt,d1,d2,d3)            { myexit(1 + __LINE__); }
#define ERROR4(v,txt,d1,d2,d3,d4)         { myexit(1 + __LINE__); }
#define ERROR5(v,txt,d1,d2,d3,d4,d5)      { myexit(1 + __LINE__); }
#define ERROR6(v,txt,d1,d2,d3,d4,d5,d6)   { myexit(1 + __LINE__); }

#define WARNING0(txt)                     { }
#define WARNING1(txt,d1)                  { }
#define WARNING2(txt,d1,d2)               { }
#define WARNING3(txt,d1,d2,d3)            { }
#define WARNING4(txt,d1,d2,d3,d4)         { }
#define WARNING5(txt,d1,d2,d3,d4,d5)      { }
#define WARNING6(txt,d1,d2,d3,d4,d5,d6)   { }

#endif

                    /* Used for fatal errors. Force exit. */
#define FATAL(p)    {error_level=0;p}


#ifdef _DEBUG
#ifndef _MSVC
#define _ASSERT(be) {if(!(be)){printf("Assertion failed on line %d in file %s\n", __LINE__, __FILE__);myexit(2);}}
#endif
#else
#define _ASSERT(be) {}
#endif 
 
/****** Data sizes *******
* U8  must be an unsigned of 8 bits
* U16 must be an unsigned of 16 bits *
* U32 must be an unsigned of 32 bits *
* U64 must be an unsigned on 64 bits *
* UPOINT must be an unsigned of the size of a pointer
* ALIGN must be the number of bytes the data accesses must be aligned to - 1
*       i.e. it must be 3 for the data to be aligned on a 32-bit boundary. It
*       must be at least 1 (16-bit alignment). It must be <= UPOINT
* ALSHIFT is log2(ALIGN + 1)
* UALIGN is the integer whose size if ALIGN + 1.
*************************/
typedef unsigned long UL;
typedef unsigned char UC;
typedef int BOOL;
#define  TRUE  1
#define  FALSE 0

#define MASK(bitlen) ((1 << (bitlen)) - 1)

#ifdef alpha_UNIX	      /* Both GCC and DEC cc seem to have the same */
#define ALSHIFT  3
#define ALIGN    MASK(ALSHIFT)
#define UALIGN   U64

#define U8     unsigned char
#define S8     char
#define U16    unsigned short
#define S16    short
#define U32    unsigned int
#define S32    int
#define U64    unsigned long
#define S64    long
#define UPOINT U64
#define SPOINT S64
#define DIRLISTSEP ':'
#endif /* alpha_UNIX */

#ifdef x86_WIN32
#define ALSHIFT 2
#define ALIGN   MASK(ALSHIFT)
#define UALIGN  U32

#define U8     unsigned char
#define S8     char
#define U16    unsigned short
#define S16    short
#define U32    unsigned long
#define S32    long
#define UPOINT U32
#define SPOINT S32
 
#define DIRLISTSEP ';'

#ifdef _MSVC  /* MSVC on x86 */
#define U64    unsigned __int64
#define S64    __int64
#endif /* _MSVC */

#ifdef _GNUCC  /* GNU CC on x86 */
#define U64    unsigned long long
#define S64    long long
#endif /* _GNUCC */ 

#endif /* x86_WIN32 */

#ifdef x86_LINUX
#define ALSHIFT 2
#define ALIGN   MASK(ALSHIFT)
#define UALIGN  U32

#define U8     unsigned char
#define S8     char
#define U16    unsigned short
#define S16    short
#define U32    unsigned long
#define S32    long
#define UPOINT U32
#define SPOINT S32
#define U64    unsigned long long
#define S64    long long
#define DIRLISTSEP ':'
#endif /* x86_WIN32 */


#ifdef _GNUCC
#define  PACKFIELD __attribute__ ((packed))
#define  min(x,y)  ((x) < (y) ? (x) : (y))
#else
#define  PACKFIELD
#endif

#define DOALIGN(addr) (((UPOINT)(addr) + ((UPOINT)ALIGN)) & (~(UPOINT)ALIGN))

#ifdef  x86_WIN32
#define CREAT(fn)    open(fn, O_CREAT  | O_TRUNC | O_WRONLY | O_BINARY, 0777)
#define OPEN(fn)     open(fn, O_RDONLY | O_BINARY) 
#else  /* x86_WIN32 */
#define CREAT(fn)    open(fn, O_CREAT  | O_TRUNC | O_WRONLY, 0777)
#define OPEN(fn)     open(fn, O_RDONLY)
#endif



/*********** Memory management macros **************/
extern  char* __stackInit;
extern  int   __mmId;
#define STACK_CHECK(category) { char __probe;\
                      long _pDepth = __stackInit - & __probe;\
                      __MM_REPORT("stack", &__probe, _pDepth, category);}

#define STACK_INIT { char __probe;\
                     __stackInit = & __probe; __mmId = 0; }

#define MALLOC(res, err, sz, type, category) {\
       long _sz = (sz);\
       (res) = (type)malloc(_sz);\
       if(! (res)) {\
           ERROR0(err, "Cannot malloc\n"); \
       }\
       __MM_REPORT("malloc", (res), _sz, category);}

#define FREE(res, category) {\
       if(res) {\
        __MM_REPORT("free", (res), 0, category);\
        free(res); }}

#define CALLOC(res, err, nrelem, sz, type, category) {\
       int _nrelem = (nrelem);\
       long _sz = (sz);\
       (res) = (type)calloc(_nrelem, _sz);\
       if(! (res)) {\
           ERROR0(err, "Cannot calloc\n"); \
       }\
       __MM_REPORT("malloc", (res), _sz * _nrelem, category);}

#define REALLOC(res, err, sz, type, category) {\
       long _sz = (sz);\
       if((res)) { __MM_REPORT("free", (res), 0, category); }\
       (res) = (type)realloc((res), _sz);\
       if(! (res)) {\
           ERROR0(err, "Cannot realloc\n"); \
       }\
       __MM_REPORT("malloc", (res), _sz, category);}

#define STRDUP(res, err, what, category) {\
       char* _what = what;\
       long _sz = strlen(_what) + 1;\
       (res) = strdup(_what);\
       if(! (res)) {\
           ERROR0(err, "Cannot strdup\n"); \
       }\
       __MM_REPORT("malloc", (res), _sz, category);}
    
#if defined(_DEBUG) || defined(_DEBUGMM)
#define __MM_REPORT(what, where, size, category) {\
       if(debugMM) {\
        SETDBGOUT; \
        printf("*MM%d: %-6s 0x%08lx %08ld %-20s %s:%d\n", \
                __mmId ++, \
                (what), (where), (long)(size), (category),__FILE__,__LINE__);\
        RESTOREOUT; } \
        }
#else
#define __MM_REPORT(what, where, size, category) { }
#endif

/****************************************************************/
#define FILELEN(fid) lseek(fid, 0, SEEK_END)  

#ifdef  x86_WIN32
#define CRLF       "\r\n"
#else
#define CRLF       "\n"
#endif

#ifdef x86_WIN32
#define TIMESTART(clk) {clk=(double)clock();}
#define TIMESTOP(clk)  {clk=1000000.0 * ((double)clock()-(clk))/CLOCKS_PER_SEC;}
#endif

#ifdef alpha_UNIX
 struct rusage ru_str;
#define TIMESTART(clk) {getrusage(RUSAGE_SELF, &ru_str);\
                        clk = ru_str.ru_utime.tv_sec * 1000000.0 + \
                              ru_str.ru_utime.tv_usec;}

#define TIMESTOP(clk) {getrusage(RUSAGE_SELF, &ru_str);\
                       clk = (ru_str.ru_utime.tv_sec * 1000000.0 + \
                             ru_str.ru_utime.tv_usec) - (clk);}
#endif

#ifdef x86_LINUX
#define TIMESTART(clk) {clk=(double)clock();}
#define TIMESTOP(clk) {clk=1000000.0 * ((double)clock()-(clk))/CLOCKS_PER_SEC;}
#endif

#define TIME_COUNT(times,prog) {int _i;unsigned long _init=poolLen;\
                          unsigned long _mem=0L;\
                          void *_mrk = MARK;\
                          maxPoolLen = _init;\
                          TIMESTART(res_time);\
                          for(_i=(times);_i>0;_i--){\
			      if(0 && poolLen != _init) {\
                                     printf("\n_i=%d,_init=%ld, poolLen=%ld",\
                                           _i,_init,poolLen);};\
                              gc(_mrk);\
			      prog;\
			      if(_mem < maxPoolLen) {_mem=maxPoolLen;}}\
                          TIMESTOP(res_time);\
                          res_time /= (double)(times);\
                          res_mem=(long)(_mem - _init);}

#define TIME_MILLISECONDS(milliseconds,prog) \
       {unsigned long _init=poolLen;\
        unsigned long _mem=0L;\
        void *_mrk = MARK;\
        double _useconds = (milliseconds) * 1000.0;\
        int _times = 1;\
        maxPoolLen = _init;\
        while(1) {\
           int _i;\
           TIMESTART(res_time);\
           for(_i=_times;_i>0;_i--){\
              gc(_mrk);\
              prog;\
              if(_mem < maxPoolLen) {_mem=maxPoolLen;}\
           }\
           TIMESTOP(res_time);\
           if(_times >= 100000 || res_time >= _useconds) {\
              break;\
           }\
           if(res_time <= 100) {\
             _times *= 100;\
           } else {\
             int _oldtimes = _times;\
             _times = (int)(_times * (_useconds / res_time));\
             if(_times == _oldtimes) { _times = _oldtimes + 1; }\
           }\
        }\
        printf("Ran %d iterations\n", _times);\
        res_time /= (double)_times;\
        res_mem=(long)(_mem - _init);\
}


#ifdef alpha_UNIX
#define MACHINE_SPEED 175.0
#define TIMEPCC(times,prog)\
  {int _i,_pcc,_pcc0,_pcc1;\
   long _init=pool_len;long _mem=0L;\
   max_pool_len=pool_len;_pcc=0;\
   for(_i=(times);_i>=0;_i--){\
	asm volatile("rpcc %0" : "=r" (_pcc0) : : "memory");\
	prog;\
	asm volatile("rpcc %0" : "=r" (_pcc1) : : "memory");\
	_pcc += (_pcc1 - _pcc0);\
	_mem += max_pool_len;}\
	res_time = _pcc / (double)(times) / MACHINE_SPEED;\
	res_mem = (long)(_mem / (times)) - _init;co}

int TimeFunction(void* function, long *time, ...);

#define TIMEFUNCTION(times,func,arg1,arg2,arg3,arg4)\
			      /* Run-once to warm the cache */\
    { int _i;long _acc_time = 0L;long _time;\
      TimeFunction(func, &_time, arg1, arg2, arg3, arg4);\ 
      for(_i=(times);_i>0;_i--) {\
	res = TimeFunction(func, &_time, arg1, arg2, arg3, arg4);\
	_acc_time += _time;\
      }\
      res_time = (double)_acc_time / (double)(times) / MACHINE_SPEED;\
    }

#endif

/*************** Expandable arrays of structs **************/
#define CHECK_EXPANDARRAY(array,size,num,eltyp,incr,cat,errval,warnmsg) {\
   if((num) >= (size)) {\
      (size) = (num) + (incr);\
      REALLOC((array), errval, (size)*sizeof(eltyp), eltyp *, cat);\
      if((size) > (incr)) {\
        warnmsg;\
      }\
   }\
}

#define FREE_EXPANDARRAY(array,size,num,cat) {\
   if(array) { FREE((array), cat); } \
   (size)  = 0;\
   (num)   = 0;}


			      /* Load modes */
#define LOAD_SFPOL      0x001
#define LOAD_PPROOF     0x004
#define LOAD_PSTATS     0x008
#define LOAD_PARSEOBJ   0x010
#define LOAD_LISTPROOFS 0x020 /* Proofs are in list form */
#define LOAD_ORACLEANN  0x040 /* Annotations are in oracel form */
/** The PCC interface **/

                              /* Initialize PCC with a given heap size and a 
                               * given safety policy */
extern  int pccInitialize(UL heapSize, char* sfpolImage, UL imageLen);

                              /* Call after pccSafetyPolicy to set the 
                               * addresses of exported symbols to their 
                               * actual values. Returns 0 if the symbol was 
                               * found, -1 if not. Do this before the 
                               * agent is loaded if you want the agent 
                               * linked to the new address of the symbol */
extern  int pccRelinkExportedSymbol(const char* name, char* addr);

			      /* Return address of symbol defined in agent,
			       * or NULL if not found. */
extern  char* pccFindAgentSymbol(const char* name);

                              /* Checks all the untrusted code in an agent. 
                               * Makes sure that all the symbols that the 
                               * safety policy requires are defined in the 
                               * agent.  NOTE:  pccCheck delays cleanup
			       * until it is called again or pccFinalize
			       * is called.  This allows pccFindAgentSymbol
			       * to access the symbol table. */
extern  int pccCheck(char* agentImage, UL imageLen);

                              /* VCGen. Must set vcFile before calling. To 
                               * be used only on the producer side to write 
                               * the VC to a file. */
#if ! defined(EMBEDDED)
extern  int pccVCGen(char* agentImage, UL imageLen);
#endif

                              /* Must be called before exit or before 
                               * another initialization */
extern  int pccFinalize(void);


extern  FILE* vcFile;         /* The VC file if we are making only the VC */
extern  FILE* globFile;       /* The global declaration file */
extern  char* codeFile;       /* The file with the compression codes */
extern  int   timeout;        /* in seconds. No timeout if 0 */
extern  int   loadMode;       /* Various flags to VCGen */
extern  int   milliseconds;   /* How many milliseconds to run VCGen (for 
                               * timing purposes). 0 means no timing  */
extern  int   debugMM;      /* To turn off the debug version of the Memory 
                               * manager */
/* The size of LF words on disk (in bits) */
#define LFWORD_SIZE    16
#define DISK_TAG_BITS   4

#if LFWORD_SIZE == 16
typedef U16   DISKLF;
#elif LFWORD_SIZE == 32
typedef U32   DISKLF;
#endif


/* Returns NULL if it cannot find the file. Otherwise returns a malloc-ed 
 * buffer where the file was copied. */
char *loadFile(const char* name, UL *fileSize);


#define USE_REDBLACK

#undef __qaddr_t	
#undef __fsid_t	
#undef __caddr_t
#undef __fd_set
