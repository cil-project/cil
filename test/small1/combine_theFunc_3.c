struct __T137280736
{
  short vtable_field_d;
  void (*vtable_field_f) ();
};
struct __Q2_4_STL11_STLP_mutex
{
};
typedef int ptrdiff_t;
typedef int FILE;
typedef int __Q3_4_STL28__char_traits_base__tm__3_ci8int_type;
typedef int __Q3_4_STL23char_traits__tm__2_c__S8int_type;
typedef __Q3_4_STL23char_traits__tm__2_c__S8int_type
  __Q3_4_STL58basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c__S8int_type;
struct __Q2_4_STL55basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c
{
  FILE *_M_get;
  struct __T137280736 *__vptr;
};
typedef __Q3_4_STL23char_traits__tm__2_c__S8int_type
  __Q3_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c8int_type;
typedef struct
  __Q2_4_STL55basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c
  __Q3_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c14streambuf_type;
struct __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c
{
  __Q3_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c14streambuf_type
    *_M_buf;
  char _M_c;
  unsigned char _M_eof;
  unsigned char _M_have_c;
};
__inline static char
equal__Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_cCFRCQ2_4_STL33istreambuf_iterator__tm__7_Z1ZZ2Z_b___0
(struct __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c const *this,
struct __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c const
*__22657_58___i);
__inline static void
_M_getc__Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_cCFv_v___0
(struct __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c const *this);
__inline static ptrdiff_t theFunc___0 (FILE const *__20468_44___f);
__inline static char
eq_int_type__Q2_4_STL28__char_traits_base__tm__3_ciSFRCZ2ZT1_b___0
  (__Q3_4_STL28__char_traits_base__tm__3_ci8int_type const *__14289_44___c1,
   __Q3_4_STL28__char_traits_base__tm__3_ci8int_type const *__14289_66___c2)
{
}
__inline static __Q3_4_STL28__char_traits_base__tm__3_ci8int_type
eof__Q2_4_STL28__char_traits_base__tm__3_ciSFv_Z2Z___0 (void)
{
}
__inline static char
to_char_type__Q2_4_STL23char_traits__tm__2_c__SSFRCi___0 (int const
							  *__14324_40___c)
{
}
__inline static int
to_int_type__Q2_4_STL23char_traits__tm__2_c__SSFRCc___0 (char const
							 *__14328_39___c)
{
}
__inline static __Q3_4_STL58basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c__S8int_type
sgetc__Q2_4_STL58basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c__SFv___0
  (struct __Q2_4_STL55basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c
   *this)
{
  struct __T137280736 *__T136310300;
  int tmp___3;
  char const *tmp___4;
  ptrdiff_t tmp___5;
  {
    tmp___5 = theFunc___0 ((FILE const *) this->_M_get);
    if (tmp___5 > 0)
      {
	tmp___3 =
	  to_int_type__Q2_4_STL23char_traits__tm__2_c__SSFRCc___0 (tmp___4);
      }
    else
      {
	__T136310300 = this->__vptr + 8;
	tmp___3 =
	  ((*
	    ((__Q3_4_STL58basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c__S8int_type (*)(struct __Q2_4_STL55basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c *)) __T136310300->vtable_field_f))) ((struct __Q2_4_STL55basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c *) ((char *) this + (int) __T136310300->vtable_field_d));
      }
    return (tmp___3);
  }
}
__inline static char
equal__Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_cCFRCQ2_4_STL33istreambuf_iterator__tm__7_Z1ZZ2Z_b___0
  (struct
   __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c
   const *this,
   struct
   __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c
   const *__22657_58___i)
{
  {
    if ((unsigned long) this->_M_buf !=
	(unsigned
	 long) ((__Q3_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c14streambuf_type *) 0))
      {
	_M_getc__Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_cCFv_v___0
	  (this);
      }
    if ((unsigned long) __22657_58___i->_M_buf !=
	(unsigned
	 long) ((__Q3_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c14streambuf_type *) 0))
      {
	_M_getc__Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_cCFv_v___0
	  (__22657_58___i);
      }
    return ((char)
	    ((int)
	     ((struct
	       __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c
	       *) this)->_M_eof ==
	     (int) ((struct
		     __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c
		     *) __22657_58___i)->_M_eof));
  }
}
__inline static void
_M_getc__Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_cCFv_v___0
  (struct
   __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c
   const *this)
{
  __Q3_4_STL28__char_traits_base__tm__3_ci8int_type __T136310232;
  __Q3_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c8int_type
    __22676_14___c;
  {
    if ((int)
	((struct
	  __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c
	  *) this)->_M_have_c)
      {
	return;
      }
    __22676_14___c =
      sgetc__Q2_4_STL58basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c__SFv___0
      (this->_M_buf);
    ((struct
      __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c
      *) this)->_M_c =
      to_char_type__Q2_4_STL23char_traits__tm__2_c__SSFRCi___0 ((int const
								 *)
								(&__22676_14___c));
    __T136310232 = eof__Q2_4_STL28__char_traits_base__tm__3_ciSFv_Z2Z___0 ();
    ((struct
      __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c
      *) this)->_M_eof =
      (unsigned char)
      eq_int_type__Q2_4_STL28__char_traits_base__tm__3_ciSFRCZ2ZT1_b___0 ((__Q3_4_STL28__char_traits_base__tm__3_ci8int_type const *) (&__22676_14___c), (__Q3_4_STL28__char_traits_base__tm__3_ci8int_type const *) (&__T136310232));
    ((struct
      __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c
      *) this)->_M_have_c = 1;
    return;
  }
}
__inline static ptrdiff_t
theFunc___0 (FILE const *__20468_44___f)
{
}


int main()
{
  theFunc___0(0);
  return 0;
}
