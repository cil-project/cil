// Example with computed labels

extern void printf(char *fmt, ...);

int main() {

  void *nextstate = && Loop;
  int x = 0;
  int acc = 0;
  int count = 5;
  
 Loop:
  if(x == 10) nextstate = && DoneOne;
  acc += x; x ++;
  goto *nextstate;
 DoneOne:

  if(acc != 11 * 10 / 2) {
    printf("Bad result: %d\n", acc);
    return 1;
  }

  if(count <= 0) return 0;

  acc = 0; x = 0;
  nextstate = && Loop;
  count --;
  
  goto *nextstate;
}
