int foo[5];

enum foo {
  l1 = 0,
  l2, l3
};

int main(int *l2) {
  {
    int * l1;
    *l1 = *l2 + l3;
  }
  {
    enum {
      l1 = 7,
      l2 = 9,
    } z;
    return l1 + l2 + l3;
  }
  *l2 = l3 + l1;
}
