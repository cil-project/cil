int main(void) {
# 1
   int x = 5, y = x;
   int z = ({ x++; L: y -= x; y;});
   return ({ goto L; 0; });
}
