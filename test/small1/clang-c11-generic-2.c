// https://github.com/llvm/llvm-project/blob/d480f968ad8b56d3ee4a6b6df5532d485b0ad01e/clang/test/Sema/conditional.c

int main() {
  _Generic(0 ? (int const *)0 : (void *)0, int const *: (void)0);
  _Generic(0 ? (int const *)0 : (void *)1, void const *: (void)0);
  _Generic(0 ? (int volatile*)0 : (void const*)1, void volatile const*: (void)0);
  _Generic(0 ? (int volatile*)0 : (void const*)0, void volatile const*: (void)0);
  return 0;
}
