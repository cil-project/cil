
struct Foo {
  int a;
  int b;
} structure;

int main()
{
  structure = ((struct Foo) {3, 4});
  return structure.a + structure.b - 7;
}

