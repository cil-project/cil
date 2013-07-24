# 1 "cilcode.tmp/ex29.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex29.c"
  int main() {
    struct mystruct {
      int a;
      int b;
    } m;
    int local;
    int arr[3];
    int *ptr;

    ptr = &local;
    m.a = local + sizeof(m) + arr[2];
    return m.a;
  }
