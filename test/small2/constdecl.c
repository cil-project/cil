// tricky const decl

// first declare the fn
static int foo(char const *a, char const *b);

// now define it using old-style args
int foo(a, b)           
  char const *a, *b;   // looks like we're not associating 'const' with 'b'?
{
  return strlen(a) + strlen(b);
}

int main()
{
  return foo("aa", "bbb") - 5;
}
