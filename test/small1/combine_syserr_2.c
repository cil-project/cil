enum __Q2_9xSysError6Reason
{
  R_NO_ERROR__9xSysError, R_FILE_NOT_FOUND__9xSysError,
    R_PATH_NOT_FOUND__9xSysError, R_ACCESS_DENIED__9xSysError,
    R_OUT_OF_MEMORY__9xSysError, R_SEGFAULT__9xSysError, R_FORMAT__9xSysError,
    R_INVALID_ARGUMENT__9xSysError, R_READ_ONLY__9xSysError,
    R_ALREADY_EXISTS__9xSysError, R_AGAIN__9xSysError, R_BUSY__9xSysError,
    R_INVALID_FILENAME__9xSysError, R_UNKNOWN__9xSysError,
    NUM_REASONS__9xSysError
};
struct __T136466748
{
};
struct __T136483464
{
};
struct __T136484052
{
  short vtable_field_d;
  short vtable_field_i;
  void (*vtable_field_f) ();
};
struct __type_info
{
};
struct __T136484220
{
  struct __type_info tinfo;
  void *p;
  void *r;
  void *s;
};
union __T136466580
{
};
struct __T136466160
{
  void (*dtor) ();
};
struct mystring
{
};
void __dl__FPv (void *foo) {}
void kill__8mystringFv (struct mystring *const p) {}
static __inline__ void __dt__8mystringFv (struct mystring *const, int);
static struct __T136483464 __T136429476[1];
extern struct __T136484220 __T_9xSysError;
extern struct __T136484052 __vtbl__Q2_3std9type_info[];
char __TID_9xSysError;
struct __T136484052 __vtbl__9xSysError[2] = {
  {((short) 0), ((short) 0), ((void (*)()) (&__T_9xSysError))}
};
struct __T136484220 __T_9xSysError = {
  {__vtbl__Q2_3std9type_info}
  , ((const char *) "xSysError"), (&__TID_9xSysError), __T136429476
};
void
sysErrorCodeString__FiPCcT2 (struct mystring *__T136741056,
			     int __25158_33_systemErrorCode,
			     const char *__25158_62_syscallName,
			     const char *__25159_60_context)
{
  static struct __T136466160 __T136424292[1] =
    { {((void (*)()) __dt__8mystringFv), ((unsigned short) 0U),
       ((unsigned short) 65535U), ((unsigned char) 0U)} };
  __dl__FPv(__T136424292);
}
static __inline__ void
__dt__8mystringFv (struct mystring *const this, int __T136740740)
{
  if (this != ((struct mystring *) 0))
    {
      kill__8mystringFv (this);
      if (__T136740740 & 1)
	{
	  __dl__FPv (((void *) this));
	}
    }
}

int main()
{
  return 0;
}
