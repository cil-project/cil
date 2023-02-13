/*

   Copyright (c) 2001-2002,
    George C. Necula    <necula@cs.berkeley.edu>
    Scott McPeak        <smcpeak@cs.berkeley.edu>
    Wes Weimer          <weimer@cs.berkeley.edu>
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

   1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

   3. The names of the contributors may not be used to endorse or promote
   products derived from this software without specific prior written
   permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
   TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 */

#include "machdep-config.h"

#include <stdio.h>
#include <string.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_WCHAR_H
#include <wchar.h>
#endif

#ifdef HAVE_STDBOOL_H
#include <stdbool.h>
#else
typedef int bool;
#endif

#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#ifdef THREAD_IS_KEYWORD_DEF
#define THREAD_IS_KEYWORD "true"
#else
#define THREAD_IS_KEYWORD "false"
#endif

#ifdef HAVE_BUILTIN_VA_LIST_DEF
#define HAVE_BUILTIN_VA_LIST "true"
#else
#define HAVE_BUILTIN_VA_LIST "false"
#endif

#ifdef _GNUCC
#define LONGLONG long long
#define VERSION __VERSION__
#define VERSION_MAJOR __GNUC__
#define VERSION_MINOR __GNUC_MINOR__

#ifdef UNDERSCORE_NAME_DEF
#define UNDERSCORE_NAME "true"
#else
#define UNDERSCORE_NAME "false"
#endif

#endif


char *underscore(char *s)
{
  char *copy = strdup(s);
  char *space = copy;

  while ((space = strchr(space, ' ')))
    *space++ = '_';

  return copy;
}

/* The type for the machine dependency structure is generated from the
   Makefile */
int main(int argc, char **argv)
{
  int env = argc == 2 && !strcmp(argv[1], "--env");
  int alignof_short, alignof_int, alignof_long, alignof_ptr, alignof_enum,
    alignof_float, alignof_float32x, alignof_float64x, alignof_double, alignof_longdouble, alignof_float128,
    alignof_floatcomplex, alignof_doublecomplex, alignof_longdoublecomplex, alignof_float128complex,
    sizeof_fun,
    alignof_fun, alignof_str, alignof_aligned, alignof_longlong,
    little_endian, char_is_unsigned, alignof_bool;

  // The alignment of a short
  {
    struct shortstruct {
      char c;
      short s;
    };
    alignof_short = (intptr_t)(&((struct shortstruct*)0)->s);
  }

  // The alignment of an int
  {
    struct intstruct {
      char c;
      int i;
    };
    alignof_int = (intptr_t)(&((struct intstruct*)0)->i);
  }

  // The alignment of a bool
  {
    struct boolstruct {
      char c;
      bool b;
    };
    alignof_bool = (intptr_t)(&((struct boolstruct*)0)->b);
  }

  // The alignment of a long
  {
    struct longstruct {
      char c;
      long l;
    };
    alignof_long = (intptr_t)(&((struct longstruct*)0)->l);
  }

  // The alignment of long long
  {
    struct longlong {
      char c;
      LONGLONG ll;
    };
    alignof_longlong = (intptr_t)(&((struct longlong*)0)->ll);
  }

  // The alignment of a ptr
  {
    struct ptrstruct {
      char c;
      int * p;
    };
    alignof_ptr = (intptr_t)(&((struct ptrstruct*)0)->p);
  }

  // The alignment of an enum
  {
    struct enumstruct {
      char c;
      enum e2 { THREE, FOUR, FIVE } e;
    };
    alignof_enum = (intptr_t)(&((struct enumstruct*)0)->e);
  }

  // The alignment of a float
  {
    struct floatstruct {
      char c;
      float f;
    };
    alignof_float = (intptr_t)(&((struct floatstruct*)0)->f);
  }

  // The alignment of a _Float32x
  {
    struct floatstruct {
      char c;
      _Float32x f;
    };
    alignof_float32x = (intptr_t)(&((struct floatstruct*)0)->f);
  }

#if __HAVE_FLOAT64X
  // The alignment of a _Float64x
  {
    struct floatstruct {
      char c;
      _Float64x f;
    };
    alignof_float64x = (intptr_t)(&((struct floatstruct*)0)->f);
  }
#endif

  // The alignment of double
  {
    struct s1 {
      char c;
      double d;
    };
    alignof_double = (intptr_t)(&((struct s1*)0)->d);
  }

  // The alignment of long  double
  {
    struct s1 {
      char c;
      long double ld;
    };
    alignof_longdouble = (intptr_t)(&((struct s1*)0)->ld);
  }

  // The alignment of float128
  {
    struct s1 {
      char c;
      _Float128 ld;
    };
    alignof_float128 = (intptr_t)(&((struct s1*)0)->ld);
  }

  // The alignment of a float complex
  {
    struct floatstruct {
      char c;
      float _Complex f;
    };
    alignof_floatcomplex = (intptr_t)(&((struct floatstruct*)0)->f);
  }

  // The alignment of double complex
  {
    struct s1 {
      char c;
      double _Complex d;
    };
    alignof_doublecomplex = (intptr_t)(&((struct s1*)0)->d);
  }

  // The alignment of long  double complex
  {
    struct s1 {
      char c;
      long double _Complex ld;
    };
    alignof_longdoublecomplex = (intptr_t)(&((struct s1*)0)->ld);
  }

  // The alignment of float128 complex
  {
    struct s1 {
      char c;
      _Float128 _Complex ld;
    };
    alignof_float128complex = (intptr_t)(&((struct s1*)0)->ld);
  }


  alignof_str = __alignof("a string");
  alignof_fun = __alignof(main);

  sizeof_fun = 0;
#ifdef __GNUC__
  sizeof_fun = sizeof(main);
#endif

  // The alignment of anything with __attribute__((aligned))
  alignof_aligned = 0;
#ifdef __GNUC__
  {
    char __attribute__((aligned)) c;
    long double  __attribute__((aligned)) ld;
    if (__alignof(c) != __alignof(ld)) {
      fprintf(stderr, "__attribute__((aligned)) has a different effect on different types.  alignments may be computed incorrectly.\n");
    }
    alignof_aligned = __alignof(c);
  }
#endif


  // Whether char is unsigned
  char_is_unsigned = ((char)0xff) > 0;

  // endianness
  {
    int e = 0x11223344;
    little_endian = (0x44 == *(char*)&e) ? 1 :
      ((0x11 == *(char*)&e) ? 0 : (exit(1), 0));
  }

  if (env)
    {
      fprintf(stderr, "Generating CIL_MACHINE machine dependency information string (for CIL)\n");
      printf("short=%d,%d int=%d,%d long=%d,%d long_long=%d,%d pointer=%d,%d "
	     "alignof_enum=%d float=%d,%d float32x=%d,%d float64x=%d,%d double=%d,%d long_double=%d,%d float128=%d,%d float_complex=%d,%d double_complex=%d,%d long_double_complex=%d,%d float128_complex=%d,%d void=%d "
	     "bool=%d,%d fun=%d,%d alignof_string=%d max_alignment=%d size_t=%s "
	     "wchar_t=%s char16_t=%s char32_t=%s char_signed=%s "
	     "big_endian=%s __thread_is_keyword=%s __builtin_va_list=%s "
	     "underscore_name=%s\n",
	     (int)sizeof(short), alignof_short, (int)sizeof(int), alignof_int,
	     (int)sizeof(long), alignof_long, (int)sizeof(long long), alignof_longlong,
	     (int)sizeof(int *), alignof_ptr,
	     alignof_enum,
	     (int)sizeof(_Float32x), alignof_float32x,
#if __HAVE_FLOAT64X
	     (int)sizeof(_Float64x), alignof_float64x,
#else
             0, 0,
#endif
	     (int)sizeof(float), alignof_float, (int)sizeof(double), alignof_double,
	     (int)sizeof(long double), alignof_longdouble,
      (int)sizeof(_Float128), alignof_float128,
       (int)sizeof(float _Complex), alignof_floatcomplex, (int)sizeof(double _Complex), alignof_doublecomplex,
	     (int)sizeof(long double _Complex), alignof_longdoublecomplex,
       (int)sizeof(_Float128 _Complex), alignof_float128complex,
       (int)sizeof(void),
	     (int)sizeof(bool), alignof_bool,
	     sizeof_fun, alignof_fun, alignof_str, alignof_aligned,
	     underscore(TYPE_SIZE_T), underscore(TYPE_WCHAR_T), underscore(TYPE_CHAR16_T), underscore(TYPE_CHAR32_T),
	     char_is_unsigned ? "false" : "true",
	     little_endian ? "false" : "true",
	     THREAD_IS_KEYWORD, HAVE_BUILTIN_VA_LIST, UNDERSCORE_NAME);
    }
  else
    {
      fprintf(stderr, "Generating machine dependency information for CIL\n");

      printf("(* Generated by code in %s *)\n", __FILE__);
      printf("\t version_major    = %d;\n", VERSION_MAJOR);
      printf("\t version_minor    = %d;\n", VERSION_MINOR);
      printf("\t version          = \"%s\";\n", VERSION);

      // Size of certain types
      printf("\t sizeof_short               = %d;\n", (int)sizeof(short));
      printf("\t sizeof_int                 = %d;\n", (int)sizeof(int));
      printf("\t sizeof_bool                = %d;\n", (int)sizeof(bool));
      printf("\t sizeof_long                = %d;\n", (int)sizeof(long));
      printf("\t sizeof_longlong            = %d;\n", (int)sizeof(LONGLONG));
      printf("\t sizeof_ptr                 = %d;\n", (int)sizeof(int *));
      printf("\t sizeof_float               = %d;\n", (int)sizeof(float));
      printf("\t sizeof_float32x            = %d;\n", (int)sizeof(_Float32x));
#if __HAVE_FLOAT64X
      printf("\t sizeof_float64x            = %d;\n", (int)sizeof(_Float64x));
#else
      printf("\t sizeof_float64x            = %d;\n", 0);
#endif
      printf("\t sizeof_double              = %d;\n", (int)sizeof(double));
      printf("\t sizeof_longdouble          = %d;\n", (int)sizeof(long double));
      printf("\t sizeof_float128            = %d;\n", (int)sizeof(_Float128));
      printf("\t sizeof_floatcomplex        = %d;\n", (int)sizeof(float _Complex));
      printf("\t sizeof_doublecomplex       = %d;\n", (int)sizeof(double _Complex));
      printf("\t sizeof_longdoublecomplex   = %d;\n", (int)sizeof(long double _Complex));
      printf("\t sizeof_float128complex     = %d;\n", (int)sizeof(_Float128 _Complex));
      printf("\t sizeof_void                = %d;\n", (int)sizeof(void));
      printf("\t sizeof_fun                 = %d;\n", (int)sizeof_fun);
      printf("\t size_t                     = \"%s\";\n", TYPE_SIZE_T);
      printf("\t wchar_t                    = \"%s\";\n", TYPE_WCHAR_T);
      printf("\t char16_t                   = \"%s\";\n", TYPE_CHAR16_T);
      printf("\t char32_t                   = \"%s\";\n", TYPE_CHAR32_T);
      printf("\t alignof_short              = %d;\n", alignof_short);
      printf("\t alignof_int                = %d;\n", alignof_int);
      printf("\t alignof_bool               = %d;\n", alignof_bool);
      printf("\t alignof_long               = %d;\n", alignof_long);
      printf("\t alignof_longlong           = %d;\n", alignof_longlong);
      printf("\t alignof_ptr                = %d;\n", alignof_ptr);
      printf("\t alignof_enum               = %d;\n", alignof_enum);
      printf("\t alignof_float              = %d;\n", alignof_float);
      printf("\t alignof_float32x           = %d;\n", alignof_float32x);
#if __HAVE_FLOAT64X
      printf("\t alignof_float64x           = %d;\n", alignof_float64x);
#else
      printf("\t alignof_float64x           = %d;\n", 0);
#endif
      printf("\t alignof_double             = %d;\n", alignof_double);
      printf("\t alignof_longdouble         = %d;\n", alignof_longdouble);
      printf("\t alignof_float128           = %d;\n", alignof_float128);
      printf("\t alignof_floatcomplex       = %d;\n", alignof_floatcomplex);
      printf("\t alignof_doublecomplex      = %d;\n", alignof_doublecomplex);
      printf("\t alignof_longdoublecomplex  = %d;\n", alignof_longdoublecomplex);
      printf("\t alignof_float128complex    = %d;\n", alignof_float128complex);
      printf("\t alignof_str                = %d;\n", alignof_str);
      printf("\t alignof_fun                = %d;\n", alignof_fun);
      printf("\t alignof_aligned            = %d;\n", alignof_aligned);
      printf("\t char_is_unsigned           = %s;\n", char_is_unsigned ? "true" : "false");
      printf("\t underscore_name            = %s;\n", UNDERSCORE_NAME);
      printf("\t __builtin_va_list          = %s;\n", HAVE_BUILTIN_VA_LIST);
      printf("\t __thread_is_keyword        = %s;\n", THREAD_IS_KEYWORD);
      printf("\t little_endian              = %s;\n", little_endian ? "true" : "false");
    }
  return 0;
}
