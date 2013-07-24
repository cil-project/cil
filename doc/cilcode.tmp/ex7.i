# 1 "cilcode.tmp/ex7.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex7.c"
int main() {
  enum {
     FIVE = 5,
     SIX, SEVEN,
     FOUR = FIVE - 1,
     EIGHT = sizeof(double)
  } x = FIVE;
 return x;
}
