
int * external(int *);


int foo(int *y) {
  int *external(int *z); // We must pull this out
  int (* local1)();  // We must leave this here
  int (* local2[4])(); // And this one as well

  local2[0] = local1; // Use them somehow
  
  return * external(y);
}

int *external(int *x) {
  return x + 1;
}



int main(void) {
  int x[2];
  x[0] = 1;
  x[1] = 7;
  return !(foo(x) == 8);
}
