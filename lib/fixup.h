/* 
 *
 * Copyright (c) 2001 by
 *  George C. Necula	necula@cs.berkeley.edu
 *  Scott McPeak        smcpeak@cs.berkeley.edu
 *  Wes Weimer          weimer@cs.berkeley.edu
 *   
 * All rights reserved.  Permission to use, copy, modify and distribute
 * this software for research purposes only is hereby granted, 
 * provided that the following conditions are met: 
 * 1. Redistributions of source code must retain the above copyright notice, 
 * this list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice, 
 * this list of conditions and the following disclaimer in the documentation 
 * and/or other materials provided with the distribution. 
 * 3. The name of the authors may not be used to endorse or promote products 
 * derived from  this software without specific prior written permission. 
 *
 * DISCLAIMER:
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR 
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*** Macros to fixup some things that the parser does not understand ***/


/* Use this in the source to cast an integer what to a pointer in the same 
 * home area as host. Use this guarded by CCURED  */
#ifdef CCURED
#define CASTTOPOINTER(btyp, host, what) \
      (((btyp *)(host)) + (((S32)(what) - (S32)(host)) / ((S32)sizeof(btyp)))) 
#else
#define CASTTOPOINTER(btyp, host, what) ((btyp *)(what))
#endif

#if 0 && !defined(__NODE) && !defined(MANUALBOX)
  #define __SAFE
  #define __INDEX
  #define __TAGGED
  #define __FSEQ
  #define __SEQ
  #define __WILD
  #define __SIZED
  #define __RWSTRING
  #define __ROSTRING
  #define __NULLTERM
  #define __SEQN
  #define __FSEQN
  #define __SAFEUNION
  #define __WILDT
  #define __FSEQT
  #define __SEQT
  #define __FSEQNT
  #define __SEQNT
  #define __INDEXT
  #define __NODE
  #define __HEAPIFY
  #define __DUMMYDEFN
  #define __BOXMODEL
  #define __NOBOXBLOCK
  #define __NOBOX
  #define __MODELEDBODY
  #define __BOXVARARG(x)
  #define __BOXFORMAT(x)
#else
#if !defined(__NODE)
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
  #define __ROSTRING  __attribute__((rostring))
  #define __RWSTRING  __attribute__((string))
  #define __SAFEUNION __attribute__((safeunion))
  #define __INDEXT   __attribute__((indext))
  #define __WILDT   __attribute__((wildt))
  #define __SEQT   __attribute__((seqt))
  #define __SEQNT   __attribute__((seqnt))
  #define __FSEQT   __attribute__((fseqt))
  #define __FSEQNT   __attribute__((fseqnt))
  #define __NODE
  #ifndef DISABLE_HEAPIFY
    #define __HEAPIFY __attribute__((heapify))
  #else
    #define __HEAPIFY
  #endif
  #define __DUMMYDEFN __attribute__((dummydefn))
  #define __BOXMODEL __attribute__((boxmodel))
  #define __NOBOXBLOCK  __blockattribute__(nobox)
  #define __NOBOX __attribute__((nobox))
  #define __MODELEDBODY
  #define __BOXVARARG(x)
  #define __BOXFORMAT(x)
#endif
#endif

// sm: I don't understand why the above macros are all conditioned
// on MANUALBOX; many of the annotations have nothing to do with
// what MANUALBOX does (as I see it); so I'll start a fledgling group
// based on just CCURED, and if someone sees this differently we can
// deal with it then
#ifdef CCURED
  #define __CANPOINTTOSTACK __attribute__((canPointToStack))
#else
  #define __CANPOINTTOSTACK
#endif

//#if ! defined(MANUALBOX) && ! defined(INFERBOX)
//#define calloc_fseq calloc
//#endif

#if !defined(CCURED)
  // if some code calls explicit_gc, but we're not boxing, then
  // we won't link ccured{debug,}lib.a either; so let's provide
  // a dummy definition of this fn

  /* But we cannot put it here since this will end up being included several 
   * times (once before patching, and one after cabs2cil) */
  //static __inline int explicit_gc() { return 0; }
  #define explicit_gc() ((void)0)
#endif

#ifdef _MSVC
#pragma warning(disable: 4068) // Unrecognized pragma
#endif

#ifdef _GNUCC
extern void *__builtin_memset (void *__s, int __c, unsigned int __n);
extern int __builtin_memcmp (__const void *__s1,
                             __const void *__s2, unsigned int __n);
extern double __builtin_fabs(double);
extern float __builtin_fabsf(float);
extern long double __builtin_fabsl(long double);

#endif

// Now specify some special pragmas
#ifdef CCURED

  //
  //Define FP_FAIL for use in wrappers:
  //
  #if !defined(RELEASELIB) && !defined(FP_FAIL_IS_VERBOSE)
    #define FP_FAIL_IS_VERBOSE
  #endif

  #if defined(FP_FAIL_IS_VERBOSE)
    #define FP_FAIL_EXTRA_PARAMS , char *file, int line
    #define FP_FAIL_EXTRA_ARGS , file, line
    #define FILE_AND_LINE , FILEFUNC, LINEFUNC
    #define FP_FAIL_STR(s) fp_fail_str(s, __FILE__, __LINE__)
    #define FP_FAIL(code) fp_fail(code, __FILE__, __LINE__)
  #else
    #define FP_FAIL_EXTRA_PARAMS
    #define FP_FAIL_EXTRA_ARGS
    #define FILE_AND_LINE
    #define FP_FAIL_STR(s) fp_fail_str(s)
    #define FP_FAIL(code) fp_fail(code)
  #endif

  #ifndef NORETURN
    #ifdef _MSVC
      #define NORETURN __declspec(noreturn)
    #else
      #define NORETURN __attribute__((noreturn))
    #endif
  #endif

  // declaration of our failure functions
  // fail with a given error message
  NORETURN  void fp_fail_str(char *str  FP_FAIL_EXTRA_PARAMS);

  // fail with a given code, which maps to a message
  NORETURN  void fp_fail(int msgId  FP_FAIL_EXTRA_PARAMS);


//////////////////////////////////////////////////////////////
// Declare primitive wrapper functions:
//

//These functions do not do any error checking:

  void * __SAFE  __ptrof_nocheck(void *ptr);
  // Type inference:  no constraints.
  // In the wrapper:  returns ptr._p.

  void * __SAFE  __startof(void *ptr); 
  // Type inference:  ptr must allow backwards arithmetic.
  // In the wrapper:  returns ptr._b.

  void * __SAFE  __endof(void *ptr);
  // Type inference:  ptr must allow forwards arithmetic.
  // In the wrapper:  returns ptr._e (WILDs: returns 0 if ptr._b == 0).

  void * __mkptr(void * __SAFE p, void *phome);
  // Type inference:  phome must be castable to the return type.
  // In the wrapper:  returns a (multiword) pointer to p with phome's
  //                    memory region.

  int __noninteger(void *ptr);
  // Type inference:  no constraints.
  // In the wrapper:  returns zero iff ptr is 0 or another non-pointer value
  //                    (i.e. an int cast to a pointer); non-zero otherwise.


//These functions may call fp_fail:

  void * __SAFE  __ptrof(void *ptr);
  // Type inference:  no constraints.
  // In the wrapper:  returns ptr._p.
  //                 Calls fp_fail if ptr is out of bounds.

  int __strlen(void *ptr);      
  // Type inference:  ptr must allow forwards arithmetic; also sets the
  //                    reachString flag.
  // In the wrapper:  returns the length of the string, not couting the 
  //                    terminating nul.
  //                  Calls fp_fail if ptr is null or not nul-terminated.

  void __write_at_least(void *ptr, unsigned int n);  
  // Type inference:  ptr must allow forwards arithmetic.
  // In the wrapper:  Verifies that we can write the next n bytes of ptr. In
  //                    WILDs, also clears the tags for the next n bytes.
  //                  Calls fp_fail if ptr is null or not long enough.

  void __copytags(void *dest, void* src, unsigned int n);  
  // Type inference:  ptr must allow forwards arithmetic.
  // In the wrapper:  Verifies that we can read/write the next n bytes
  //                    of both dest and src.  In WILDs, also copys the
  //                    appropriate tags from src to dest.
  //                  Calls fp_fail if either dest or src is null or not long
  //                    enough.


  #pragma boxpoly("__ptrof")
  #pragma boxpoly("__ptrof_nocheck")
  #pragma boxpoly("__startof")
  #pragma boxpoly("__endof")
  #pragma boxpoly("__mkptr")
  #pragma boxpoly("__strlen")
  #pragma boxpoly("__noninteger")
  #pragma boxpoly("__write_at_least")
  #pragma boxpoly("__copytags")

  //Helper routine:
  static inline
  void __assert_noninteger(void *ptr) {
    if (!__noninteger(ptr)){
      FP_FAIL(15/*FAIL_NONPOINTER*/);
    }
  }


  // there's more of these in ccured_GNUCC.patch..
  #pragma boxvararg_printf("printf", 1)
  #pragma boxvararg_printf("vprintf", 1)      // sm: this fixes a problem with rbtest when rmtmps is disabled ...
  #pragma boxvararg_printf("fprintf", 2)

  #pragma boxpoly("ccured_kind_of")
  char *  ccured_kind_of(void *);
  #pragma boxalloc("malloc", nozero, sizein(1))
  #pragma boxpoly("free")
  #pragma boxalloc("alloca", nozero, sizein(1))
  #pragma boxalloc("calloc", zero, sizemul(1,2))

  // sm: not sure best way to handle this.. gcc's headers map all
  // these things to __builtin_blah, so we see that after
  // preprocessing..  could attack with patcher, but what if someone
  // writes __buildin_blah in their code?  we'll see how this goes
  #ifdef _GNUCC
    void *__builtin_alloca(unsigned int size);
    #pragma boxalloc("__builtin_alloca", nozero, sizein(1))
    // waiting on rest until need them..
  #endif

/*
  // sm: taking a stab at strchr's model
  static inline
  char* strchr_model(char* dest, int c)
  {
    return dest;      // just establish the flow
  }
  #pragma cilnoremove("strchr_model")
  #pragma boxmodelof("strchr_model", "strchr")

  static inline char *strdup_model(char const *s)
  {
    char *p;
    __endof(s);                  // need a length
    return p;                    // result is unconstrained new value
  }
  #pragma cilnoremove("strdup_model")
  #pragma boxmodelof("strdup_model", "strdup")
*/
  // sm: I cannot force return value to be fseq if 's' is ...
  static inline char *strpbrk_model(const char *s, const char *accept)
  {
    __endof(s);          // s must be searchable
    __endof(accept);     // also accept
    return s;            // return points into 's'
  }
  #pragma cilnoremove("strpbrk_model")
  #pragma boxmodelof("strpbrk_model", "strpbrk")

  static inline char *strtok_model(char *s, char const *delim)
  {
    __endof(s);
    __endof(delim);
    return s;
  }
  #pragma cilnoremove("strtok_model")
  #pragma boxmodelof("strtok_model", "strtok")

  #pragma boxpoly("memcpy")
  #pragma boxpoly("memset", "__builtin_memset")
  #pragma boxpoly("memmove")
  #pragma boxpoly("memcmp")
  #pragma boxpoly("write")
  #pragma boxpoly("read")
  #pragma boxpoly("fread")
  #pragma boxpoly("fwrite")
  #pragma boxpoly("mmap")      // sm: for ftpd
  #pragma boxpoly("munmap")    // sm: for ftpd

  #pragma boxpoly("memset_seq_model")
  static inline
  void* memset_seq_model(void* dest, int towrite, unsigned int size)
  {
    void *end = __endof(dest); // Make sure it has an end
    return dest;
  }
  #pragma boxmodelof("memset_seq_model", "memset", "__builtin_memset")
  #pragma cilnoremove("memset_seq_model")



  #pragma boxpoly("memcpy_seq_model")
  static inline
  void* memcpy_seq_model(void* dest, void *src, unsigned int size)
  {
    void *end = __endof(dest);
    dest = src; // Make sure they are both have the same type
    return dest;
  }
  #pragma boxmodelof("memcpy_seq_model", "memcpy", "memmove",
                     "__builtin_memcpy")
  #pragma cilnoremove("memcpy_seq_model")



  // like for allocators, we have __builtin_blah for str*...
  #ifdef _GNUCC
    #pragma boxpoly("__builtin_memcpy")
    void *__builtin_memcpy(void *dest, const void *src, unsigned int n);
  #endif

  static inline
  void qsort_seq_model(void *base, unsigned int nmemb, unsigned int size,
          int (*compar)(const void *, const void *))
  {
      void *end = __endof(base);                    
      // sm:flow 'base' type into compar's arguments, otherwise
      // it might let 'base' be seq while compar takes wild (this
      // happens for ptrdist-1.1/anagram)
      compar(base, base);
      return;
  }
  #pragma boxpoly("qsort")
  #pragma boxmodelof("qsort_seq_model", "qsort")
  #pragma cilnoremove("qsort_seq_model")

// Whenever you use MAKE_QSORT(foosort, ...) also add
// #pragma cilnoremove("foosort_seq_model")
// #pragma boxmodelof("foosort_seq_model", foosort)
//
  #define MAKE_QSORT(name,namestr,elt_type) \
  static inline\
  void name (elt_type *base, unsigned int nmemb, unsigned int size,\
          int (*compar)(const elt_type *, const elt_type *));\
  static inline\
  void name ## _seq_model(elt_type *base, unsigned int nmemb, unsigned int size,\
          int (*compar)(const elt_type *, const elt_type *)) \
  {\
      elt_type *end = __endof(base);\
      return;\
  }


  #pragma boxexported("main")
     

  // stuff for test/small2/ioctl.c
  union ioctl_format {
    int anInt;
    int *anIntPtr;
  };
  #pragma boxvararg("ioctl", sizeof(union ioctl_format))

  // for test/small2/execv.c
  static inline int execv_model(char *path, char **argv)
  {
    // make sure I can tell how long the 'path' and 'argv' arrays are
    __endof(path);
    __endof(argv);
    return 0;
  }
  #pragma boxmodelof("execv_model", "execv")

  // for ping; need a model so I get to write a wrapper, where
  // I can emulate optarg as optarg_f
  static inline int getopt_model(int argc, char **argv, char const *optstring)
  {
    // need length of argv
    __endof(argv);
    return 0;
  }
  #pragma boxmodelof("getopt_model", "getopt")

  // for EDG-generated code, which uses this function purely to fool gcc's
  // optimizer: the address of certain variables is passed to this thing,
  // which simply ignores them
  //void __suppress_optim_on_vars_in_try(void *foo, ...);
  union suppress_optim_format {
    void *anyPtr;
  };
  #pragma boxvararg("__suppress_optim_on_vars_in_try", sizeof(union suppress_optim_format))

#endif



// sm: I think it's a bad idea to try to match signal's declaration since it's
// such an unusual type; and it doesn't use any types that aren't built-in

// gn: disabled this since everything in BOX mode fails due to redefin.
#ifdef CCURED
  typedef void (*_box_sig_fn)(int);
  static inline
  _box_sig_fn signal_model(int signum, _box_sig_fn fn)
  {
    // flow argument to result
    return fn;
  }
  #pragma cilnoremove("signal_model")
  #pragma boxmodelof("signal_model", "signal")

#endif // CCURED


#ifndef CCURED
  #define __startof(p) p
  #define __endof(p) p
#endif


#ifdef CCURED
/* We add a mechanism for casting arbitrarily without penalty. This is unsound. 
 * Use only when you know what you are doing. */
#pragma boxpoly("trusted_cast")
void * trusted_cast(void * p);
#else
#define trusted_cast(p)       (p)
#endif

