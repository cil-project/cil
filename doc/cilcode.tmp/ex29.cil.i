# 1 "cilcode.tmp/ex29.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex29.cil.c"
# 2 "cilcode.tmp/ex29.c"
struct mystruct {
   int a ;
   int b ;
};
# 1 "cilcode.tmp/ex29.c"
int main(void)
{
  struct mystruct m ;
  int local ;
  int arr[3] ;
  int *ptr ;
  unsigned long __cil_tmp5 ;
  unsigned long __cil_tmp6 ;
  int __cil_tmp7 ;
  unsigned long __cil_tmp8 ;
  int *__cil_tmp9 ;
  int __cil_tmp10 ;
  unsigned long __cil_tmp11 ;
  unsigned long __cil_tmp12 ;
  unsigned long __cil_tmp13 ;
  int m_b14 ;
  int m_a15 ;

  {
# 10 "cilcode.tmp/ex29.c"
  ptr = & local;
# 11 "cilcode.tmp/ex29.c"
  __cil_tmp5 = 2 * 4UL;
# 11 "cilcode.tmp/ex29.c"
  __cil_tmp6 = (unsigned long )(arr) + __cil_tmp5;
# 11 "cilcode.tmp/ex29.c"
  __cil_tmp7 = *((int *)__cil_tmp6);
# 11 "cilcode.tmp/ex29.c"
  __cil_tmp8 = (unsigned long )__cil_tmp7;
# 11 "cilcode.tmp/ex29.c"
  __cil_tmp9 = & local;
# 11 "cilcode.tmp/ex29.c"
  __cil_tmp10 = *__cil_tmp9;
# 11 "cilcode.tmp/ex29.c"
  __cil_tmp11 = (unsigned long )__cil_tmp10;
# 11 "cilcode.tmp/ex29.c"
  __cil_tmp12 = __cil_tmp11 + 8UL;
# 11 "cilcode.tmp/ex29.c"
  __cil_tmp13 = __cil_tmp12 + __cil_tmp8;
# 11 "cilcode.tmp/ex29.c"
  m_a15 = (int )__cil_tmp13;
# 12 "cilcode.tmp/ex29.c"
  return (m_a15);
}
}
