static void *jtab[2]; // A jump table
static int doit(int x){
 
  static int jtab_init = 0;
  if(!jtab_init) { // Initialize the jump table
    jtab[0] = &&lbl1;
    jtab[1] = &&lbl2;
    jtab_init = 1;
  }
  goto *jtab[x]; // Jump through the table
lbl1:
  return 0;
lbl2:
  return 1;
}
 
int main(void){
  if (doit(0) != 0) exit(1);
  if (doit(1) != 1) exit(1);
  exit(0);
}
