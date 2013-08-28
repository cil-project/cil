# 1 "cilcode.tmp/ex24.cil.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex24.cil.c"



extern void * stackguard_get_ra();
extern void stackguard_set_ra(void *new_ra);



struct stackguard_stack {
  void * data;
  struct stackguard_stack * next;
} * stackguard_stack;

void stackguard_push(void *ra) {
  void * old = stackguard_stack;
  stackguard_stack = (struct stackguard_stack *)
    malloc(sizeof(stackguard_stack));
  stackguard_stack->data = ra;
  stackguard_stack->next = old;
}

void * stackguard_pop() {
  void * ret = stackguard_stack->data;
  void * next = stackguard_stack->next;
  free(stackguard_stack);
  stackguard_stack->next = next;
  return ret;
}
# 3 "cilcode.tmp/ex24.c"
extern int ( scanf)() ;
# 1 "cilcode.tmp/ex24.c"
int dangerous(void)
{
  char array[10] ;
  void *return_address ;

  {
  return_address = (void *)stackguard_get_ra();
  stackguard_push(return_address);
# 3 "cilcode.tmp/ex24.c"
  scanf("%s", array);
  {
  return_address = (void *)stackguard_pop();
  stackguard_set_ra(return_address);
# 4 "cilcode.tmp/ex24.c"
  return (0);
  }
}
}
# 6 "cilcode.tmp/ex24.c"
int main(void)
{
  int tmp ;

  {
# 7 "cilcode.tmp/ex24.c"
  tmp = dangerous();
# 7 "cilcode.tmp/ex24.c"
  return (tmp);
}
}
