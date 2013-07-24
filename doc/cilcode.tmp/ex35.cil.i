# 1 "cilcode.tmp/ex35.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex35.cil.c"
# 1 "cilcode.tmp/ex35.c"
struct __anonstruct_T1_1 {
   char *key ;
   char *value ;
};
# 1 "cilcode.tmp/ex35.c"
typedef struct __anonstruct_T1_1 T1;
# 7 "cilcode.tmp/ex35.c"
struct __anonstruct_T3_2 {
   long type ;
   char *value ;
};
# 7 "cilcode.tmp/ex35.c"
typedef struct __anonstruct_T3_2 T3;
# 13 "cilcode.tmp/ex35.c"
static T3 __constr_expr_0 = {1L, (char *)1};
# 13 "cilcode.tmp/ex35.c"
T1 a[1] = { {(char *)"", (char *)(& __constr_expr_0)}};
# 20 "cilcode.tmp/ex35.c"
int main(void)
{
  T3 *pt3 ;

  {
# 21 "cilcode.tmp/ex35.c"
  pt3 = (T3 *)a[0].value;
# 22 "cilcode.tmp/ex35.c"
  return ((int )pt3->value);
}
}
