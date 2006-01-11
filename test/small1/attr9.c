//Some uses of attributes in initramfs.c in the 2.6 linux kernel:

//The attribute should go on the variables, not the enum type.
static __attribute__ ((__section__ (".init.data"))) enum state {
 Start,
 Collect,
 GotHeader,
 SkipIt,
 GotName,
 CopyFile,
 GotSymlink,
 Reset
} state, next_state;




//This attribute belongs to the function, not the return type:
int __attribute__((noinline)) inflate_fixed(void){
  return 0;
}


int main() {
  state = Reset;
  return 0;
}
