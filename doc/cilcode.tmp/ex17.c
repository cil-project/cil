int main(void) {
# 1
  int x;
  int y = x ? 2 : 4;
  int z = x || y;
  // Here we duplicate the return statement
  if(x && y) { return 0; } else { return 1; }
  // To avoid excessive duplication, CIL uses goto's for 
  // statement that have more than 5 instructions
  if(x && y || z) { x ++; y ++; z ++; x ++; y ++; return z; }
}
