/* bitvectori.c */
/* C implementation of some of bitvector.mli */

#include <caml/alloc.h>         /* caml_alloc */
#include <caml/mlvalues.h>      /* value */
#include <caml/fail.h>          /* caml_invalid_argument */
#include <string.h>             /* memset, memcpy */
#include <assert.h>             /* assert */
#include <stdio.h>              /* printf (for debugging) */


#if 0
  #define debugf(x) printf x
#else
  #define debugf(x) ((void)0)
#endif

enum { BITS_PER_WORD = sizeof(unsigned long) * 8 };


/* map a bitvector 'value' to a pointer to its bits */
inline unsigned long *getBits(value vec)
{
  return (unsigned long*)Op_val(vec);
}

/* map a bitvector 'value' to the # of words of bits it has */
inline long getNumWords(value vec)
{
  return Wosize_val(vec);
}


value bitvector_create(value n_)
{
  int bits = Int_val(n_);
  int words;
  value ret;

  if (bits < 0) {
    caml_invalid_argument("Negative bitvector size.");
  }

  /* divide, rounding up */
  words = (bits + BITS_PER_WORD-1) / BITS_PER_WORD;

  debugf(("bitvector_create: bits=%d, words=%d\n", bits, words));

  /* allocate */
  ret = caml_alloc(words, No_scan_tag);
  assert(getNumWords(ret) >= words);

  /* zero */
  memset(getBits(ret), 0, words * sizeof(unsigned long));

  return ret;
}


value bitvector_length(value vec)
{
  long words = getNumWords(vec);
  debugf(("bitvector_length: words=%ld\n", words));
  return Val_long(words * BITS_PER_WORD);
}


void bitvector_copyBits(value dest, value src)
{
  long srcWords = getNumWords(src);
  long destWords = getNumWords(dest);
  long words = (srcWords<destWords? srcWords : destWords);

  unsigned long const *srcBits = getBits(src);
  unsigned long *destBits = getBits(dest);

  memcpy(destBits, srcBits, words * sizeof(unsigned long));
}


void bitvector_clearAll(value vec)
{
  long words = getNumWords(vec);
  unsigned long *bits = getBits(vec);

  memset(bits, 0, words * sizeof(unsigned long));
}


/* given vector 'vec' and bit 'n', set 'bits' to point at the
 * word containing the bit, and 'n' to the bit number */
#define OFFSET_CALCULATION                    \
  unsigned long *bits = getBits(vec);         \
  long words = getNumWords(vec);              \
  if (n < 0 || n > words * BITS_PER_WORD) {   \
    caml_array_bound_error();                 \
  }                                           \
  bits += n / BITS_PER_WORD;                  \
  n = n % BITS_PER_WORD /* user ; */


value bitvector_test(value vec, value n_)
{
  int n = Int_val(n_);
  int bit;

  OFFSET_CALCULATION;

  bit = (*bits >> n) & 1;
  return Val_int(bit);
}


void bitvector_set(value vec, value n_)
{
  int n = Int_val(n_);

  OFFSET_CALCULATION;

  *bits |= (1L << n);
}


void bitvector_clear(value vec, value n_)
{
  int n = Int_val(n_);

  OFFSET_CALCULATION;

  *bits &= ~(1L << n);
}


void bitvector_setTo(value vec, value n_, value bit_)
{
  if (Int_val(bit_)) {
    bitvector_set(vec, n_);
  }
  else {
    bitvector_clear(vec, n_);
  }
}


void bitvector_unioneq(value a, value b)
{
  long aWords = getNumWords(a);
  long bWords = getNumWords(b);

  unsigned long *aBits = getBits(a);
  unsigned long const *bBits = getBits(b);

  while (aWords && bWords) {
    *aBits |= *bBits;
    aBits++;
    bBits++;
    aWords--;
    bWords--;
  }

  /* any excess bits in 'a' are left as-is */
}


void bitvector_intersecteq(value a, value b)
{
  long aWords = getNumWords(a);
  long bWords = getNumWords(b);

  unsigned long *aBits = getBits(a);
  unsigned long const *bBits = getBits(b);

  while (aWords && bWords) {
    *aBits &= *bBits;
    aBits++;
    bBits++;
    aWords--;
    bWords--;
  }

  /* any excess bits in 'a' are zeroed, under the premise that
   * the missing bits of 'b' should be treated as zero and this
   * is an intersection operation */
  while (aWords) {
    *aBits = 0;
    aBits++;
    aWords--;
  }
}



void bitvector_complementeq(value a)
{
  long aWords = getNumWords(a);
  unsigned long *aBits = getBits(a);

  while (aWords) {
    *aBits = ~*aBits;
    aBits++;
    aWords--;
  }
}


#undef OFFSET_CALCULATION


/* EOF */
