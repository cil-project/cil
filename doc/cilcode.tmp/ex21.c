int main(void) {
# 1
   int x, y, z;
   return &(x ? y : z) - & (x ++, x);
}
