int foo[5];

enum foo {
  l1 = 0,
  l2, l3
};


typedef struct Person {
  char *name;
} Person;


int main() {
  int l, *l2 = & l;
  Person *Person = 0;
  {
    int * l1 = l2;
    *l1 = *l2 + l3;
  }
  {
    enum {
      l1 = 7,
      l2 = 9,
    } z;
    return (l1 + l2 + l3) - 18;
  }
  *l2 = l3 + l1;
}
