# 1 "cilcode.tmp/ex48.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex48.c"
int main(void) {
# 1 "cilcode.tmp/ex48.c"
struct {
  int x;
  struct {
     int y, z;
     struct {
       int u, v;
     };
 };
} a;
return a.x + a.y + a.z + a.u + a.v;
}
