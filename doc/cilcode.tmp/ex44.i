# 1 "cilcode.tmp/ex44.c"
# 1 "<command-line>"
# 1 "cilcode.tmp/ex44.c"
extern inline foo(void) { return 1; }
int firstuse(void) { return foo(); }


int foo(void) { return 2; }

int main() {
    return foo() + firstuse();
}
