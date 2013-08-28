# 1 "cilcode.tmp/ex20.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex20.c"
int main(void) {
# 1 "cilcode.tmp/ex20.c"
   int x = 5, y = x;
   int z = ({ x++; L: y -= x; y;});
   return ({ goto L; 0; });
}
