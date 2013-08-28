int main(void) {
# 1
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
