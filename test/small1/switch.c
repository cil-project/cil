
extern void exit(int);
extern int printf(const char*, ...);

// Testing some ugly switch cases
int foo(int x, int y) {
  switch(x) {
    y = "who runs this?"[3];
    exit(1);
  default:
    y ++;
    goto L1;
  case 1:
  L2:
  case 20:
    y ++;
    break;
    y ++;
  L1:
    if(y > 5) {
    case 7:
      x ++;
    } else {
      while(x < 33) {
      case 9:
        x ++;
        break;
      }
      break;
    }

    goto L2;
  }
  if(x < 30)
    goto L1;
  return x + y;
}


int main() {
  int res =
    foo(1, 2) +
    17 * foo(9, 5) +
    126 * foo(7, 2) +
    3037 * foo(15, 9);
  printf("Result is: %d\n", res);
  if(res != 171821)
    exit(1);
  return 0;
}
