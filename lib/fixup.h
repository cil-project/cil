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

#include "ccuredannot.h"


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
extern int __builtin_memcmp (const void *__s1,
                             const void *__s2, unsigned int __n);
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
  int  ccured_kind_of(void *);

  #pragma boxalloc("malloc", nozero, sizein(1))
  #pragma boxpoly("free")
  #pragma boxalloc("alloca", nozero, sizein(1))
  #pragma boxalloc("calloc", zero, sizemul(1,2))

  /* for the models that need it:
  void * __SAFE  __endof(void *ptr);
  #pragma boxpoly("__endof")
  */
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
//  #define MAKE_QSORT(name,namestr,elt_type) \
//  static inline\
//  void name (elt_type *base, unsigned int nmemb, unsigned int size,\
//          int (*compar)(const elt_type *, const elt_type *));\
//  static inline\
//  void name ## _seq_model(elt_type *base, unsigned int nmemb, unsigned int size,\
//          int (*compar)(const elt_type *, const elt_type *)) \
//  {\
//      elt_type *end = __endof(base);\
//      return;\
//  }


  #pragma boxexported("main")
     
  // for ping; need a model so I get to write a wrapper, where
  // I can emulate optarg as optarg_f
  // put this in the include file where getopt occurs
//  static inline int getopt_model(int argc, char **argv, char const *optstring)
//  {
//    // need length of argv
//    __endof(argv);
//    return 0;
//  }
//  #pragma boxmodelof("getopt_model", "getopt")


#endif


#ifdef CCURED
/* We add a mechanism for casting arbitrarily without penalty. This is unsound. 
 * Use only when you know what you are doing. */
#pragma boxpoly("trusted_cast")
void * trusted_cast(void * p);

/* Like a trusted cast but the input is an integer along with a size */
#pragma boxpoly("trusted_cast_int")
void * trusted_cast_int(unsigned long p, unsigned int size);


#else
#define trusted_cast(p)             (p)
#define trusted_cast_int(p, size)   ((void*)p)
#endif

