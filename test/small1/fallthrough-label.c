int main() {
  int a =5;

  switch(a) {
      case 5:
          __attribute__((__fallthrough__));
      default:
          a = 8;
  }

  return 0;
}
