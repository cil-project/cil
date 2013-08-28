# 1 "cilcode.tmp/ex4.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex4.c"
int main() {
  struct foo {
        int x; } foo;
  {
     struct foo {
        double d;
     };
     return foo.x;
  }
}
