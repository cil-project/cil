# 1 "cilcode.tmp/ex45.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex45.cil.c"
# 1 "cilcode.tmp/ex45.c"
struct s {
   int i1 ;
   int i2 ;
};
# 1 "cilcode.tmp/ex45.c"
union u {
   int i ;
   struct s s ;
};
# 8 "cilcode.tmp/ex45.c"
union u x = {6};
# 10 "cilcode.tmp/ex45.c"
int main(void)
{
  struct s y ;
  union u z ;

  {
# 11 "cilcode.tmp/ex45.c"
  y.i1 = 1;
# 11 "cilcode.tmp/ex45.c"
  y.i2 = 2;
# 12 "cilcode.tmp/ex45.c"
  z.s = y;
# 13 "cilcode.tmp/ex45.c"
  return (0);
}
}
