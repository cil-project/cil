int main() {
  int a =5;

  switch(a) {
      case 5:
        a = 8;
        __attribute__((__fallthrough__));
      case 10:
        __attribute__((__fallthrough__));
      default:
        a = 8;
  }

  return 0;
}
