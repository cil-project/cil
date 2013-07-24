# 1 "cilcode.tmp/ex25.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex25.cil.c"
# 3 "cilcode.tmp/ex25.c"
extern int ( scanf)() ;
# 1 "cilcode.tmp/ex25.c"
struct dangerous_heapify {
   char array[10] ;
};
# 1 "cilcode.tmp/ex25.c"
int dangerous(void)
{
  struct dangerous_heapify *dangerous_heapify ;
  int __cil_tmp3 ;

  {
# 1 "cilcode.tmp/ex25.c"
  dangerous_heapify = (struct dangerous_heapify *)malloc(sizeof(struct dangerous_heapify ));
# 3 "cilcode.tmp/ex25.c"
  scanf("%s", dangerous_heapify->array);
  {
# 4 "cilcode.tmp/ex25.c"
  __cil_tmp3 = 0;
# 4 "cilcode.tmp/ex25.c"
  free(dangerous_heapify);
# 4 "cilcode.tmp/ex25.c"
  return (__cil_tmp3);
  }
}
}
# 6 "cilcode.tmp/ex25.c"
int main(void)
{
  int tmp ;

  {
# 7 "cilcode.tmp/ex25.c"
  tmp = dangerous();
# 7 "cilcode.tmp/ex25.c"
  return (tmp);
}
}
