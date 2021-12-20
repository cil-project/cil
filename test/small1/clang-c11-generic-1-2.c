// https://github.com/llvm/llvm-project/blob/d480f968ad8b56d3ee4a6b6df5532d485b0ad01e/clang/test/Sema/generic-selection.c

int main() {
  (void) _Generic(0,
      char: 0, short: 0, long: 0);
  return 0;
}
