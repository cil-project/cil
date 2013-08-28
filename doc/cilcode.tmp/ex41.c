int main(void) {
# 1
  extern int f();
  return f() ? : -1; // Returns the result of f unless it is 0
}
