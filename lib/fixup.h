/*** Macros to fixup some things that the parser does not understand ***/


/* Use this in the source to cast an integer what to a pointer in the same 
 * home area as host. Use this guarded by BEFOREBOX  */
#ifdef BEFOREBOX
#define CASTTOPOINTER(btyp, host, what) \
      (((btyp *)(host)) + (((S32)(what) - (S32)(host)) / ((S32)sizeof(btyp)))) 
#else
#define CASTTOPOINTER(btyp, host, what) ((btyp *)(what))
#endif

#if !defined(__NODE) && !defined(MANUALBOX)
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
  #define __BOXMODEL(fname)
  #define __NOBOXBLOCK
  #define __MODELLED
  #define __BOXVARARG(x)
  #define __BOXFORMAT(x)
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
  #define __BOXMODEL(fname) __attribute__((boxmodel(fname)))
  #define __NOBOXBLOCK  __blockattribute__(nobox)
  #define __MODELLED
  #define __BOXVARARG(x)
  #define __BOXFORMAT(x)
#endif

//#if ! defined(MANUALBOX) && ! defined(INFERBOX)
//#define calloc_fseq calloc
//#endif

#if !defined(BEFOREBOX)
  // if some code calls explicit_gc, but we're not boxing, then
  // we won't link safec{debug,}lib.a either; so let's provide
  // a dummy definition of this fn

  /* But we cannot put it here since this will end up being included several 
   * times (once before patching, and one after cabs2cil) */
  //static __inline int explicit_gc() { return 0; }
  #define explicit_gc() ((void)0)
#endif

#ifdef _MSVC
#pragma warning(disable: 4068) // Unrecognized pragma
#endif



// Now specify some special pragmas
#ifdef BEFOREBOX
  #pragma boxalloc("malloc", nozero, sizein(0))
  #pragma boxalloc("alloca", nozero, sizein(0))
  #pragma boxalloc("calloc", zero, sizemul(0,1))
  
  union printf_format {
    int             f_int;
    double          f_double;
    char * __ROSTRING f_string;
  };
  #define PRINTF_FUNCTION(name, format_idx) _Pragma boxvararg_printf(name, sizeof(union printf_format), format_idx)

  PRINTF_FUNCTION("printf", 0)
  PRINTF_FUNCTION("fprintf", 1)
  PRINTF_FUNCTION("snprintf", 2)
  PRINTF_FUNCTION("syslog", 1)
  PRINTF_FUNCTION("sprintf", 1)

  // We want to force sprintf to carry a length
  static inline
  int sprintf_model(char *buffer, const char *format, ...)
     __BOXMODEL("sprintf");
  #pragma boxvararg("sprintf_model", sizeof(union printf_format))
  static inline 
  int sprintf_model(char *buffer, const char *format, ...) {
    // Force buffer to carry a length
    void* e = __endof(buffer); // buffer ++ would do the same
    return 0;
  }
  
  #pragma boxpoly("memcpy")
  #pragma boxpoly("memset")
  #pragma boxpoly("memmove")

  #pragma boxpoly("memset_seq_model")
  static inline
  void* memset_seq_model(void* dest, int towrite, unsigned int size)
     __BOXMODEL("memset");     
  static inline
  void* memset_seq_model(void* dest, int towrite, unsigned int size)
  {
    void *end = __endof(dest); // Make sure it has an end
    return dest;
  }

  #pragma boxpoly("memcpy_seq_model")
  static inline
  void* memcpy_seq_model(void* dest, void *src, unsigned int size)
       __BOXMODEL("memcpy") __BOXMODEL("memmove");
  static inline 
  void* memcpy_seq_model(void* dest, void *src, unsigned int size)
  {
    void *end = __endof(dest);
    dest = src; // Make sure they are both have the same type
    return dest;
  }

#pragma boxexported("main")
#endif


// ideally we could handle this better..
// hack: 'restrict' is a problem with glibc 2.2
#define __restrict
#define restrict


// sm: I think it's a bad idea to try to match signal's declaration since it's
// such an unusual type; and it doesn't use any types that aren't built-in

// gn: disabled this since everythign in BOX mode fails due to redefin.
#ifdef BEFOREBOX
  typedef void (*_box_sig_fn)(int);
  static inline
  _box_sig_fn signal_model(int signum, _box_sig_fn fn)
         __BOXMODEL("signal");
  static inline
  _box_sig_fn signal_model(int signum, _box_sig_fn fn)
  {
    // flow argument to result
    return fn;
  }


  static inline
  char *strpbrk_model(char const *s, char const *accept) __BOXMODEL("strpbrk");
  static inline 
  char *strpbrk_model(char const *s, char const *accept)
  {
    int someInt = (int)(*accept);   // make sure 'accept' can be read from
    return s;                       // connect s to retval
  }

#endif // BEFOREBOX

#ifdef BEFOREBOX
#pragma boxpoly("__endof")
void *__endof(void *ptr); // Get the end of a pointer
#pragma boxpoly("__startof")
void *__startof(void *ptr); // Get the start of a pointer
#else
#define __startof(p) p
#define __endof(p) p
#endif


