enum {A, B, C} foo() {
  return A;
}

void bar() {
  if (foo() == A) {
  }
}

int main() {
  return 0;
}
