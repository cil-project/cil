struct __T137280736
{
  short vtable_field_d;
  void (*vtable_field_f) ();
};
typedef struct pthread_mutex_t pthread_mutex_t;
struct __Q2_4_STL20_STLP_mutex_indirect
{
};
struct __Q2_4_STL11_STLP_mutex
{
};
struct __Q2_4_STL6locale
{
  struct __Q2_4_STL12_Locale_impl *_M_impl;
};
typedef int ptrdiff_t;
struct __quad_t
{
};
typedef struct _IO_FILE FILE;
struct _IO_FILE
{
};
typedef int __Q3_4_STL23char_traits__tm__2_c__S8int_type;
typedef __Q3_4_STL23char_traits__tm__2_c__S8int_type
  __Q3_4_STL58basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c__S8int_type;
struct __Q2_4_STL55basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c
{
  FILE *_M_get;
  struct __T137280736 *__vptr;
};
typedef struct
  __Q2_4_STL55basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c
  __Q3_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c14streambuf_type;
struct __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c
{
  __Q3_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c14streambuf_type
    *_M_buf;
  unsigned char _M_have_c;
};
__inline static ptrdiff_t _FILE_I_avail__4_STLFPC8_IO_FILE (FILE const
							    *__20454_44___f) {}
__inline static int
to_int_type__Q2_4_STL23char_traits__tm__2_c__SSFRCc (char const
						     *__14314_39___c)
{
}
__inline static
  __Q3_4_STL58basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c__S8int_type
sbump
(struct __Q2_4_STL55basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c *this);
__inline static char *_FILE_I_postincr__4_STLFP8_IO_FILE (FILE *
			        			  __20458_37___f) {}
__inline static __Q3_4_STL58basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c__S8int_type
sbump
  (struct __Q2_4_STL55basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c
   *this)
{
  struct __T137280736 *__T136310300;
  int tmp___3;
  char const *tmp___4;
  ptrdiff_t tmp___5;
  {
    tmp___5 = _FILE_I_avail__4_STLFPC8_IO_FILE ((FILE const *) this->_M_get);
    if (tmp___5 > 0)
      {
	tmp___4 =
	  (char const *) _FILE_I_postincr__4_STLFP8_IO_FILE (this->_M_get);
	tmp___3 =
	  to_int_type__Q2_4_STL23char_traits__tm__2_c__SSFRCc (tmp___4);
      }
    else
      {
	__T136310300 = this->__vptr + 9;
	tmp___3 =
	  ((*
	    ((__Q3_4_STL58basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c__S8int_type (*)(struct __Q2_4_STL55basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c *)) __T136310300->vtable_field_f))) ((struct __Q2_4_STL55basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c *) ((char *) this + (int) __T136310300->vtable_field_d));
      }
    return (tmp___3);
  }
}
__inline static void
_M_bumpc__Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_cFv_v___0
(struct __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c *this);
__inline static __Q3_4_STL58basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c__S8int_type
sbump___0
  (struct __Q2_4_STL55basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c
   *this)
{
  struct __T137280736 *__T136310300;
  int tmp___3;
  char const *tmp___4;
  ptrdiff_t tmp___5;
  {
    tmp___5 = _FILE_I_avail__4_STLFPC8_IO_FILE ((FILE const *) this->_M_get);
    if (tmp___5 > 0)
      {
	tmp___4 =
	  (char const *) _FILE_I_postincr__4_STLFP8_IO_FILE (this->_M_get);
	tmp___3 =
	  to_int_type__Q2_4_STL23char_traits__tm__2_c__SSFRCc (tmp___4);
      }
    else
      {
	__T136310300 = this->__vptr + 9;
	tmp___3 =
	  ((*
	    ((__Q3_4_STL58basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c__S8int_type (*)(struct __Q2_4_STL55basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c *)) __T136310300->vtable_field_f))) ((struct __Q2_4_STL55basic_streambuf__tm__32_cQ2_4_STL20char_traits__tm__2_c *) ((char *) this + (int) __T136310300->vtable_field_d));
      }
    return (tmp___3);
  }
}
__inline static void
_M_bumpc__Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_cFv_v___0
  (struct
   __Q2_4_STL59istreambuf_iterator__tm__32_cQ2_4_STL20char_traits__tm__2_c
   *this)
{
  {
    sbump___0
      (this->_M_buf);
    this->_M_have_c = 0;
    return;
  }
}


int main(int argc, char *argv)
{
  if (argc > 100) {    // gcc doesn't know this is always false
    sbump___0(0);      // if actually called, this segfaults
  }
  return 0;
}
