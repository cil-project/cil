extern void __dl__FPv (void *);
extern void kill__8mystringFv (struct mystring *const);
static __inline__ void
__dt__8mystringFv (struct mystring *const this, int __T136762132)
{
  if (this != ((struct mystring *) 0))
    {
      kill__8mystringFv (this);
      if (__T136762132 & 1)
	{
	  __dl__FPv (((void *) this));
	}
    }
}
