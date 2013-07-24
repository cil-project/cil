# 1 "cilcode.tmp/ex34.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex34.c"
struct {
   int x;
   struct {
       int y, z;
   } nested;
} i = { .nested.y = 5, 6, .x = 1, 2 };
