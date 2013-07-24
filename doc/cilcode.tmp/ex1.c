int long signed x;
signed long extern x;
long static int long y;

// Some code that uses these declaration, so that CIL does not remove them
int main() { return x + y; }
