

enum foo {
  e0 = sizeof(int),
  e1,
  e2 = e0 + 1
};

int useenum(enum foo x) {
  switch(x) {
  case e1: return 0;
  case e2 * 2: return 1;
    
  }
}
  
TESTDEF "enum1" : success ~ case +sizeof *\( *int *\)
TESTDEF "enum2" : success ~ e2 *\* *2

int main() {
  return 0;
}
