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

  //for the models that need it:
  void * __SAFE  __endof(void *ptr);
  #pragma boxpoly("__endof")

  // sm: not sure best way to handle this.. gcc's headers map all
  // these things to __builtin_blah, so we see that after
  // preprocessing..  could attack with patcher, but what if someone
  // writes __buildin_blah in their code?  we'll see how this goes
  #ifdef _GNUCC
    void *__builtin_alloca(unsigned int size);
    #pragma boxalloc("__builtin_alloca", nozero, sizein(1))
    // waiting on rest until need them..
  #endif

  #pragma boxpoly("mmap")      // sm: for ftpd
  #pragma boxpoly("munmap")    // sm: for ftpd

  // like for allocators, we have __builtin_blah for str*...
  #ifdef _GNUCC
    #pragma boxpoly("__builtin_memcpy")
    void *__builtin_memcpy(void *dest, const void *src, unsigned int n);
  #endif

/*
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
*/
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
     
/*
  // stuff for test/small2/ioctl.c
  union ioctl_format {
    int anInt;
    int *anIntPtr;
  };
  #pragma boxvararg("ioctl", sizeof(union ioctl_format))
*/
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

