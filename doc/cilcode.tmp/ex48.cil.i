# 1 "cilcode.tmp/ex48.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex48.cil.c"
# 1 "cilcode.tmp/ex48.c"
struct __anonstruct____missing_field_name_3 {
   int u ;
   int v ;
};
# 1 "cilcode.tmp/ex48.c"
struct __anonstruct____missing_field_name_2 {
   int y ;
   int z ;
   struct __anonstruct____missing_field_name_3 __annonCompField1 ;
};
# 1 "cilcode.tmp/ex48.c"
struct __anonstruct_a_1 {
   int x ;
   struct __anonstruct____missing_field_name_2 __annonCompField2 ;
};
# 1 "cilcode.tmp/ex48.c"
int main(void)
{
  struct __anonstruct_a_1 a ;

  {
# 10 "cilcode.tmp/ex48.c"
  return ((((a.x + a.__annonCompField2.y) + a.__annonCompField2.z) + a.__annonCompField2.__annonCompField1.u) + a.__annonCompField2.__annonCompField1.v);
}
}
