extern inline foo(void) { return 1; }
int firstuse(void) { return foo(); }

// A second, incompatible definition of foo
int foo(void) { return 2; }

int main() {
    return foo() + firstuse();
}
