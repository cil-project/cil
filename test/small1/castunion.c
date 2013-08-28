union X {
	int a;
	short b;
};

int main()
{
	union X u,v;
	v = (union X) u;
    return 0;
}

