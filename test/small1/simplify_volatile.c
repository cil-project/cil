struct M {
	int foo[1];
} volatile m;

int main()
{
    return &m.foo;
}
