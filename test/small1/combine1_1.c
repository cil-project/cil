
struct str1 {
  int x1;
  int x2;
} array;

extern void printf(char *, ...);
#define E(n) { printf("Error %d\n", n); return (n); }

extern int c2(void), c3(void);
int main() {
  int c1res = sizeof(array);
  int c2res = c2();
  int c3res = c3();

  if(c1res != c3res) E(1);

  if(c2res != sizeof(int [10]) + sizeof(int)) E(2);

  printf("Success\n");
  return 0;
}
